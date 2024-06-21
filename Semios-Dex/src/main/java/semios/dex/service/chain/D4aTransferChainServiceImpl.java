package semios.dex.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.dex.model.dto.common.Dao4ArtDexConstant;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.request.InfuraCallRequestDto;
import semios.dex.model.dto.response.TransactionDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.UserLiquidityStatistics;
import semios.dex.model.enums.ContractMethodEnum;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.IUserLiquidityStatisticsService;
import semios.dex.service.SubscriberChainService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * 交易对的流动
 *
 * @description: 交易对的流动
 * @author: xiangbin
 * @create: 2023-05-17
 **/
@Slf4j
@Service
@Deprecated
public class D4aTransferChainServiceImpl implements SubscriberChainService {

    //列子：https://goerli.etherscan.io/tx/0xb6d1521c5fc85d5aa89821c8007137007ceca1b841a71a27ef95ac1b5881e070#eventlog

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ISubscriptionService subscriptionService;

    @Autowired
    private IUserLiquidityStatisticsService userLiquidityStatisticsService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[D4aTransferChainServiceImpl] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());

        String fromAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        String toAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String value = CommonUtil.hexToTenString(transactionDto.getData());

        log.info("[D4aTransferChainServiceImpl] transactionHash:{} fromAddress:{} toAddress:{} value:{}", transactionDto.getTransactionHash(), fromAddress, toAddress, value);

        if (value == null || StringUtils.isAnyBlank(fromAddress, toAddress, value)) {
            log.info("[D4aTransferChainServiceImpl] null return");
            return;
        }
        if (Dao4ArtDexConstant.ZERO_ADDRESS.equals(fromAddress) && Dao4ArtDexConstant.ZERO_ADDRESS.equals(toAddress)) {
            //都是零地址的不记录
            log.info("[D4aTransferChainServiceImpl] all zero_address return");
            return;
        }
        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByPairAddress(transactionDto.getContractAddress());
        if (erc20Liquidity == null) {
            log.error("[D4aTransferChainServiceImpl] cannot find Erc20Liquidity transactionDao:{}", JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("cannot find Erc20Liquidity");
        }
        //查询pair合约totalSupply();
        InfuraCallRequestDto indexRequest = new InfuraCallRequestDto();
        indexRequest.setNetWork(Dao4ArtDexConstant.netWork);
        indexRequest.setTo(erc20Liquidity.getPairAddress());
        indexRequest.setData(ContractMethodEnum.TOTAL_SUPPLY.getMethodAddress());

        Result<String> indexResult = subscriptionService.infuraCall(indexRequest);
        if (indexResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[D4aTransferChainServiceImpl] pairAddress:{} index error result:{}", erc20Liquidity.getPairAddress(), indexResult.getResultDesc());
        } else {
            String indexResultData = indexResult.getData();
            String pairAmount = CommonUtil.hexToTenString(indexResultData);
            if (StringUtils.isBlank(pairAmount)) {
                log.error("[D4aTransferChainServiceImpl] pairAddress:{} pairAmount is null", erc20Liquidity.getPairAddress());
            } else {
                BigDecimal pairAmountDecimal = new BigDecimal(pairAmount).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                log.info("[D4aTransferChainServiceImpl] pairAddress:{} pairAmount:{} newPairAmount:{}", erc20Liquidity.getPairAddress(), erc20Liquidity.getPairBalance(), pairAmountDecimal);
                log.info("[D4aTransferChainServiceImpl] pairAddress:{} erc20Liquidity:{}", erc20Liquidity.getPairAddress(), JacksonUtil.obj2json(erc20Liquidity));
                erc20Liquidity.setPairBalance(pairAmountDecimal);
                erc20LiquidityService.updateById(erc20Liquidity);
            }
        }

        List<UserLiquidityStatistics> userLiquidityStatisticsList = new ArrayList<>();
        //给toAddress增加流动性代币
        if (!Dao4ArtDexConstant.ZERO_ADDRESS.equals(toAddress)) {
            UserLiquidityStatistics toUserLiquidityStatistics = userLiquidityStatisticsService.selectByUserAddress(toAddress, erc20Liquidity.getErc20Address());
            if (toUserLiquidityStatistics == null) {
                BigDecimal valueDecimal = new BigDecimal(value).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                //这里用不用新建用户
                toUserLiquidityStatistics = new UserLiquidityStatistics();
                toUserLiquidityStatistics.setErc20Address(erc20Liquidity.getErc20Address());
                toUserLiquidityStatistics.setUserAddress(toAddress);
                log.info("[D4aTransferChainServiceImpl] transactionHash:{} toUserAddress:{} new poolToken:{} ", transactionDto.getTransactionHash(), toUserLiquidityStatistics.getUserAddress(), valueDecimal);
            }
            //查询pair合约toAddress的余额
            InfuraCallRequestDto indexRequest1 = new InfuraCallRequestDto();
            indexRequest1.setNetWork(Dao4ArtDexConstant.netWork);
            indexRequest1.setTo(erc20Liquidity.getPairAddress());
            indexRequest1.setData(ContractMethodEnum.BALANCE_OF.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(toAddress)));

            Result<String> indexResult1 = subscriptionService.infuraCall(indexRequest1);
            if (indexResult1.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[D4aTransferChainServiceImpl] toAddress index error result:{}", indexResult1.getResultDesc());
                throw new RuntimeException(transactionDto.getContractAddress() + "查询pair余额异常");
            } else {
                String indexResultData = indexResult1.getData();
                String pairAmount = CommonUtil.hexToTenString(indexResultData);
                log.info("[D4aTransferChainServiceImpl] toAddress:{} transactionHash:{} pairAmount:{} pairAddress:{}", toAddress, transactionDto.getTransactionHash(), pairAmount, transactionDto.getContractAddress());
                if (StringUtils.isBlank(pairAmount)) {
                    throw new RuntimeException(transactionDto.getContractAddress() + " pairAmount not find");
                }
                BigDecimal pairAmountDecimal = new BigDecimal(pairAmount).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                log.info("[D4aTransferChainServiceImpl] transactionHash:{} toAddress:{} pairAmountDecimal:{}", transactionDto.getTransactionHash(), toUserLiquidityStatistics.getUserAddress(), pairAmountDecimal);
                toUserLiquidityStatistics.setPoolToken(pairAmountDecimal);
            }

            userLiquidityStatisticsList.add(toUserLiquidityStatistics);
        }

        if (!Dao4ArtDexConstant.ZERO_ADDRESS.equals(fromAddress)) {
            //给fromAddress减少流动性代币
            UserLiquidityStatistics fromUserLiquidityStatistics = userLiquidityStatisticsService.selectByUserAddress(fromAddress, erc20Liquidity.getErc20Address());
            if (fromUserLiquidityStatistics == null) {
                log.error("[D4aTransferChainServiceImpl] cannot find fromUserLiquidityStatistics fromAddress:{} erc20_address:{}", fromAddress, transactionDto.getContractAddress());
                fromUserLiquidityStatistics = new UserLiquidityStatistics();
                fromUserLiquidityStatistics.setErc20Address(fromAddress);
                fromUserLiquidityStatistics.setUserAddress(transactionDto.getContractAddress());
//                throw new RuntimeException("cannot find fromUserLiquidityStatistics");
            }
            //查询pair合约fromAddress的余额
            InfuraCallRequestDto indexRequestFrom = new InfuraCallRequestDto();
            indexRequestFrom.setNetWork(Dao4ArtDexConstant.netWork);
            indexRequestFrom.setTo(erc20Liquidity.getPairAddress());
            indexRequestFrom.setData(ContractMethodEnum.BALANCE_OF.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(fromAddress)));

            Result<String> indexResultFrom = subscriptionService.infuraCall(indexRequestFrom);
            if (indexResultFrom.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[D4aTransferChainServiceImpl] fromAddress index error result:{}", indexResultFrom.getResultDesc());
                throw new RuntimeException(transactionDto.getContractAddress() + "查询pair余额异常");
            } else {
                String indexResultData = indexResultFrom.getData();
                String pairAmount = CommonUtil.hexToTenString(indexResultData);
                log.info("[D4aTransferChainServiceImpl] fromAddress:{} transactionHash:{} pairAmount:{} pairAddress:{}", fromAddress, transactionDto.getTransactionHash(), pairAmount, transactionDto.getContractAddress());
                if (StringUtils.isBlank(pairAmount)) {
                    throw new RuntimeException(transactionDto.getContractAddress() + " pairAmount not find");
                }
                BigDecimal pairAmountDecimal = new BigDecimal(pairAmount).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                log.info("[D4aTransferChainServiceImpl] transactionHash:{} fromUserAddress:{} pairAmountDecimal:{}", transactionDto.getTransactionHash(), fromUserLiquidityStatistics.getUserAddress(), pairAmountDecimal);
                fromUserLiquidityStatistics.setPoolToken(pairAmountDecimal);
            }
            userLiquidityStatisticsList.add(fromUserLiquidityStatistics);

        }

        userLiquidityStatisticsService.saveOrUpdateBatch(userLiquidityStatisticsList);

    }

}
