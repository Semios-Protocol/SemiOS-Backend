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
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.enums.ContractMethodEnum;
import semios.dex.model.enums.LiquidityTradeTypeEnum;
import semios.dex.model.enums.OneOrZeroEnum;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ILiquidityTransactionService;
import semios.dex.service.SubscriberChainService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;

/**
 * 减少流动性
 *
 * @description: 减少流动性
 * @author: xiangbin
 * @create: 2023-05-17
 **/
@Slf4j
@Service
public class D4aBurnChainServiceImpl implements SubscriberChainService {

    // 例子：https://goerli.etherscan.io/tx/0xc2a76406bf1900e2c5cfaad3a92cf27d91c14b387e9c8c2b5a67947508e78f2b#eventlog

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ISubscriptionService subscriptionService;

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[D4ABurnChainServiceImpl] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String pairAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String amount0 = CommonUtil.hexToTenString(dataList.get(0));
        String amount1 = CommonUtil.hexToTenString(dataList.get(1));

        log.info("[D4ABurnChainServiceImpl] transactionHash:{} amount0:{} amount1:{} pairAddress:{}", transactionDto.getTransactionHash(), amount0, amount1, pairAddress);

        if (amount0 == null || amount1 == null || StringUtils.isAnyBlank(amount0, amount1)) {
            throw new RuntimeException(pairAddress + " is Illegal transaction");
        }

        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByPairAddress(pairAddress);
        if (erc20Liquidity == null) {
            throw new RuntimeException(pairAddress + " Erc20Liquidity not find");
        }
        LiquidityTransaction liquidityTransaction = liquidityTransactionService.selectByTransactionHashAndType(erc20Liquidity.getErc20Address(), transactionDto.getTransactionHash(), LiquidityTradeTypeEnum.REMOVE.getStatus());
        if (liquidityTransaction != null) {
            log.info("[D4ABurnChainServiceImpl] had handle transactionHash:{} pairAddress:{}", transactionDto.getTransactionHash(), pairAddress);
            return;
        }


        liquidityTransaction = new LiquidityTransaction();
        liquidityTransaction.setErc20Address(erc20Liquidity.getErc20Address());
        liquidityTransaction.setErc20Name(erc20Liquidity.getErc20Name());
        liquidityTransaction.setErc20Symbol(erc20Liquidity.getErc20Symbol());
        liquidityTransaction.setTradeType(LiquidityTradeTypeEnum.REMOVE.getStatus());
        liquidityTransaction.setTransactionHash(transactionDto.getTransactionHash());
        liquidityTransaction.setBlockTime(Long.valueOf(transactionDto.getBlockTime()));

        String inputTokenAddress = getDaoInputToken(erc20Liquidity.getProjectId());
        log.info("[D4ABurnChainServiceImpl] get input token projectId{},inputTokenAddress:{}", erc20Liquidity.getProjectId(), inputTokenAddress);
        String decimal = getDecimals(inputTokenAddress);
        log.info("[D4ABurnChainServiceImpl] get input token projectId{},inputTokenAddress:{},decimal:{}", erc20Liquidity.getProjectId(), inputTokenAddress, decimal);
        if (StringUtils.isBlank(decimal)) {
            decimal = "18";
        }

        BigDecimal amount0Decimal = new BigDecimal(amount0).divide(CommonUtil.getPowBigDecimal(Integer.valueOf(decimal)), 18, RoundingMode.FLOOR);   // Dao4ArtDexConstant.BASIC_RATIO
        BigDecimal amount1Decimal = new BigDecimal(amount1).divide(CommonUtil.getPowBigDecimal(Integer.valueOf(decimal)), 18, RoundingMode.FLOOR); // Dao4ArtDexConstant.BASIC_RATIO
        if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ONE.getStatus())) {
            liquidityTransaction.setInTokenAmount(amount1Decimal);
            liquidityTransaction.setOutTokenAmount(amount0Decimal);
            liquidityTransaction.setEthAmount(amount0Decimal);
        } else {
            liquidityTransaction.setInTokenAmount(amount0Decimal);
            liquidityTransaction.setOutTokenAmount(amount1Decimal);
            liquidityTransaction.setEthAmount(amount1Decimal);
        }


        // 查询创建者
        Result<String> result = subscriptionService.ethGetTransactionByHash(Dao4ArtDexConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[D4ABurnChainServiceImpl] ethGetTransactionByHash result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[D4ABurnChainServiceImpl] ethGetTransactionByHash error result:{}", result.getResultDesc());
            throw new RuntimeException(transactionDto.getTransactionHash() + "cannot find creator");
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[D4ABurnChainServiceImpl] objectMap is null result:{}", result.getData());
            throw new RuntimeException(transactionDto.getTransactionHash() + "cannot find creator");
        }
        String from = (String) objectMap.get("from");
        log.info("[D4ABurnChainServiceImpl] ethGetTransactionByHash creator:{}", from);
        liquidityTransaction.setUserAddress(from.toLowerCase());

        erc20Liquidity.setLastSwapHash(transactionDto.getTransactionHash());
        liquidityTransactionService.saveLiquidityTransactionAndUpdateErc20Liquidity(liquidityTransaction, null);


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
