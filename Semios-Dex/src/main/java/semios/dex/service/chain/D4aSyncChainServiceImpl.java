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
import semios.dex.model.enums.ContractMethodEnum;
import semios.dex.model.enums.OneOrZeroEnum;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.SubscriberChainService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * 最新储备量
 *
 * @description: 最新储备量
 * @author: xiangbin
 * @create: 2023-05-17
 **/
@Slf4j
@Service
public class D4aSyncChainServiceImpl implements SubscriberChainService {

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ISubscriptionService subscriptionService;

    public static void main(String[] args) {
        String indexResultData = "0x000000000000000000000000000000000000000000002a5a058fc2ad3567a5c000000000000000000000000000000000000000000000000002c68af0bb127a8c0000000000000000000000000000000000000000000000000000000064609c80";
        indexResultData = CommonUtil.removeHexPrefixIfExists(indexResultData);
        List<String> indexResultDataList = CommonUtil.splitBy32Bytes(indexResultData);
        String toke0Amount = CommonUtil.hexToTenString(indexResultDataList.get(0));
        String toke1Amount = CommonUtil.hexToTenString(indexResultDataList.get(1));
        System.out.println(toke0Amount);
        System.out.println(toke1Amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[D4aSyncChainServiceImpl] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String pairAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String amount0 = CommonUtil.hexToTenString(dataList.get(0));
        String amount1 = CommonUtil.hexToTenString(dataList.get(1));

        log.info("[D4aSyncChainServiceImpl] transactionHash:{} amount0:{} amount1:{} pairAddress:{}", transactionDto.getTransactionHash(), amount0, amount1, pairAddress);

        if (amount0 == null || amount1 == null || StringUtils.isAnyBlank(amount0, amount1)) {
            throw new RuntimeException(pairAddress + " is Illegal transaction");
        }

        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByPairAddress(pairAddress);
        if (erc20Liquidity == null) {
            throw new RuntimeException(pairAddress + " Erc20Liquidity not find");
        }
//        if (StringUtils.isBlank(erc20Liquidity.getLastSwapHash()) || !erc20Liquidity.getLastSwapHash().equals(transactionDto.getTransactionHash())) {
//            log.warn("[D4aSyncChainServiceImpl] not match lastSwapHash erc20Hash:{} transactionHash:{}", erc20Liquidity.getLastSwapHash(), transactionDto.getTransactionHash());
//        }

        String inputTokenAddress = getDaoInputToken(erc20Liquidity.getProjectId());
        log.info("[D4aSyncChainServiceImpl] get input token projectId{},inputTokenAddress:{}", erc20Liquidity.getProjectId(), inputTokenAddress);
        String decimal = getDecimals(inputTokenAddress);
        log.info("[D4aSyncChainServiceImpl] get input token projectId{},inputTokenAddress:{},decimal:{}", erc20Liquidity.getProjectId(), inputTokenAddress, decimal);
        if (StringUtils.isBlank(decimal)) {
            decimal = "18";
        }


        //查询pair合约token0余额
        InfuraCallRequestDto indexRequest = new InfuraCallRequestDto();
        indexRequest.setNetWork(Dao4ArtDexConstant.netWork);
        indexRequest.setTo(pairAddress);
        indexRequest.setData(ContractMethodEnum.GET_RESERVES.getMethodAddress());

        Result<String> indexResult = subscriptionService.infuraCall(indexRequest);
        if (indexResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[D4aSyncChainServiceImpl] index error result:{}", indexResult.getResultDesc());
            throw new RuntimeException(pairAddress + "查询toke0异常");
        } else {

            String indexResultData = indexResult.getData();
            indexResultData = CommonUtil.removeHexPrefixIfExists(indexResultData);
            List<String> indexResultDataList = CommonUtil.splitBy32Bytes(indexResultData);
            String toke0Amount = CommonUtil.hexToTenString(indexResultDataList.get(0));
            String toke1Amount = CommonUtil.hexToTenString(indexResultDataList.get(1));
            log.info("[D4aSyncChainServiceImpl] transactionHash:{} amount0:{} toke0Amount:{} pairAddress:{}", transactionDto.getTransactionHash(), amount0, toke0Amount, pairAddress);
            log.info("[D4aSyncChainServiceImpl] transactionHash:{} amount1:{} toke1Amount:{} pairAddress:{}", transactionDto.getTransactionHash(), amount1, toke1Amount, pairAddress);
            if (StringUtils.isBlank(toke0Amount) || StringUtils.isBlank(toke1Amount)) {
                throw new RuntimeException(pairAddress + " tokeAmount not find");
            }
            BigDecimal token0Decimal = new BigDecimal(toke0Amount).divide(CommonUtil.getPowBigDecimal(Integer.valueOf(decimal)), 18, RoundingMode.FLOOR);
            BigDecimal token1Decimal = new BigDecimal(toke1Amount).divide(CommonUtil.getPowBigDecimal(Integer.valueOf(decimal)), 18, RoundingMode.FLOOR);
            if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ZERO.getStatus())) {
                log.info("[D4aSyncChainServiceImpl] transactionHash:{} erc20Address:{} erc20Balance:{} newBalacne:{}", transactionDto.getTransactionHash(), erc20Liquidity.getErc20Address(), erc20Liquidity.getErc20Balance(), token0Decimal);
                log.info("[D4aSyncChainServiceImpl] transactionHash:{} erc20Address:{} ethBalance:{} newBalacne:{}", transactionDto.getTransactionHash(), erc20Liquidity.getErc20Address(), erc20Liquidity.getEthBalance(), token1Decimal);
                erc20Liquidity.setErc20Balance(token0Decimal);
                erc20Liquidity.setEthBalance(token1Decimal);
            } else {
                log.info("[D4aSyncChainServiceImpl] transactionHash:{} erc20Address:{} erc20Balance:{} newBalacne:{}", transactionDto.getTransactionHash(), erc20Liquidity.getErc20Address(), erc20Liquidity.getErc20Balance(), token1Decimal);
                log.info("[D4aSyncChainServiceImpl] transactionHash:{} erc20Address:{} ethBalance:{} newBalacne:{}", transactionDto.getTransactionHash(), erc20Liquidity.getErc20Address(), erc20Liquidity.getEthBalance(), token0Decimal);
                erc20Liquidity.setEthBalance(token0Decimal);
                erc20Liquidity.setErc20Balance(token1Decimal);
            }
        }

        erc20Liquidity.setLastSwapHash(transactionDto.getTransactionHash());

        erc20LiquidityService.updateById(erc20Liquidity);
    }

    private String getDaoInputToken(String projectId) throws Exception {
        // 根据project id 查询绑定的input token
        // 然后根据input token查询decimal
        InfuraCallRequestDto indexRequest = new InfuraCallRequestDto();
        indexRequest.setNetWork(Dao4ArtDexConstant.netWork);
        indexRequest.setTo(Dao4ArtDexConstant.protocolContract);
        indexRequest.setData(ContractMethodEnum.DAO_INPUT_TOKEN.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(projectId)));

        Result<String> inputTokenResult = subscriptionService.infuraCall(indexRequest);
        log.info("[getDaoInputToken]projectId:{} resultData:{}", projectId, inputTokenResult.getData());
        if (inputTokenResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[getDaoInputToken]projectId:{} error result:{}", projectId, inputTokenResult.getResultDesc());
        } else {
            List<String> dataList = CommonUtil.splitBy32Bytes(inputTokenResult.getData());
            return CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(0)));
        }
        return null;
    }

    private String getDecimals(String address) {
        if (Dao4ArtDexConstant.ZERO_ADDRESS.equals(address)) {
            return "18";
        }
        try {
            log.info("[getDecimals]address:{} ", address);
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(Dao4ArtDexConstant.netWork);
            infuraCallRequestDto.setTo(address);
            infuraCallRequestDto.setData(ContractMethodEnum.DECIMALS.getMethodAddress());
            infuraCallRequestDto.setBlockNumber(null);
            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            log.info("[getDecimals]address:{} resultData:{}", address, result.getData());
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[getDecimals]address:{} error result:{}", address, result.getResultDesc());
                return null;
            }
            List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
            String erc20Balance = CommonUtil.hexToTenString(dataList.get(0));

            log.info("[getDecimals]address:{}  address:{} ", address, erc20Balance);

            return erc20Balance;
        } catch (Exception e) {
            log.error("[getDecimals]address:{} exception e:{}", address, e);
        }
        log.info("[getDecimals]address:{} return ZERO ", address);
        return "18";
    }

}
