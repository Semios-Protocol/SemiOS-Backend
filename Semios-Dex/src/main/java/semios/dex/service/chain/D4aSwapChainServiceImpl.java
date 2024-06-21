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
 * 兑换交易
 *
 * @description: 兑换交易
 * @author: xiangbin
 * @create: 2023-05-17
 **/
@Slf4j
@Service
public class D4aSwapChainServiceImpl implements SubscriberChainService {
    //例子：https://goerli.etherscan.io/tx/0x246bc3a51e66eabacc77ad82fd9493958269aa5b59e037a5756789be629c416c#eventlog

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ISubscriptionService subscriptionService;

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    public static BigDecimal getBigDecimal(String price, String decimal) {
        if (StringUtils.isBlank(price)) {
            return BigDecimal.ZERO;
        }
        if (StringUtils.isBlank(decimal)) {
            return new BigDecimal(price).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
        }
        return new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(Integer.valueOf(decimal)), 18, RoundingMode.FLOOR);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[D4aSwapChainServiceImpl] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String pairAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());

        //swap两种情况，一种用eth换erc20，一种用erc20换eth
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String amount0In = CommonUtil.hexToTenString(dataList.get(0));
        String amount1In = CommonUtil.hexToTenString(dataList.get(1));
        String amount0Out = CommonUtil.hexToTenString(dataList.get(2));
        String amount1Out = CommonUtil.hexToTenString(dataList.get(3));
        if (amount0In == null || amount1In == null || amount0Out == null || amount1Out == null) {
            return;
        }

        log.info("[D4aSwapChainServiceImpl] transactionHash:{} amount0In:{} amount1In:{} amount0Out:{} amount1Out:{} pairAddress:{}", transactionDto.getTransactionHash(), amount0In, amount1In, amount0Out, amount1Out, pairAddress);

        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByPairAddress(pairAddress);
        if (erc20Liquidity == null) {
            throw new RuntimeException(pairAddress + " Erc20Liquidity not find");
        }

        String inputTokenAddress = getDaoInputToken(erc20Liquidity.getProjectId());
        log.info("[D4aSwapChainServiceImpl] get input token projectId{},inputTokenAddress:{}", erc20Liquidity.getProjectId(), inputTokenAddress);
        String decimal = getDecimals(inputTokenAddress);
        log.info("[D4aSwapChainServiceImpl] get input token projectId{},inputTokenAddress:{},decimal:{}", erc20Liquidity.getProjectId(), inputTokenAddress, decimal);


        LiquidityTransaction liquidityTransaction = new LiquidityTransaction();
        liquidityTransaction.setErc20Address(erc20Liquidity.getErc20Address());
        liquidityTransaction.setErc20Name(erc20Liquidity.getErc20Name());
        liquidityTransaction.setErc20Symbol(erc20Liquidity.getErc20Symbol());

        liquidityTransaction.setTransactionHash(transactionDto.getTransactionHash());
        liquidityTransaction.setBlockTime(Long.valueOf(transactionDto.getBlockTime()));
        Integer tradeType = LiquidityTradeTypeEnum.SWAPERC20.getStatus();
        if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ZERO.getStatus()) && new BigDecimal(amount0In).compareTo(BigDecimal.ZERO) > 0) {
            //erc20换eth
            tradeType = LiquidityTradeTypeEnum.SWAPETH.getStatus();
            liquidityTransaction.setInTokenAmount(getBigDecimal(amount0In, decimal));
            liquidityTransaction.setOutTokenAmount(getBigDecimal(amount1Out, decimal));
            liquidityTransaction.setEthAmount(getBigDecimal(amount1Out, decimal));
        }
        if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ZERO.getStatus()) && new BigDecimal(amount1In).compareTo(BigDecimal.ZERO) > 0) {
            //eth换erc20
            liquidityTransaction.setInTokenAmount(getBigDecimal(amount1In, decimal));
            liquidityTransaction.setOutTokenAmount(getBigDecimal(amount0Out, decimal));
            liquidityTransaction.setEthAmount(getBigDecimal(amount1In, decimal));
        }
        if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ONE.getStatus()) && new BigDecimal(amount0In).compareTo(BigDecimal.ZERO) > 0) {
            //eth换erc20
            liquidityTransaction.setInTokenAmount(getBigDecimal(amount0In, decimal));
            liquidityTransaction.setOutTokenAmount(getBigDecimal(amount1Out, decimal));
            liquidityTransaction.setEthAmount(getBigDecimal(amount0In, decimal));
        }
        if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ONE.getStatus()) && new BigDecimal(amount1In).compareTo(BigDecimal.ZERO) > 0) {
            //erc20换eth
            tradeType = LiquidityTradeTypeEnum.SWAPETH.getStatus();
            liquidityTransaction.setInTokenAmount(getBigDecimal(amount1In, decimal));
            liquidityTransaction.setOutTokenAmount(getBigDecimal(amount0Out, decimal));
            liquidityTransaction.setEthAmount(getBigDecimal(amount0Out, decimal));
        }
        liquidityTransaction.setTradeType(tradeType);

        LiquidityTransaction liquidityTran = liquidityTransactionService.selectByTransactionHashAndType(erc20Liquidity.getErc20Address(), transactionDto.getTransactionHash(), tradeType);
        if (liquidityTran != null) {
            log.info("[D4aSwapChainServiceImpl] had handle transactionHash:{} pairAddress:{} tradeType:{}", transactionDto.getTransactionHash(), pairAddress, tradeType);
            return;
        }

        // 查询创建者
        Result<String> result = subscriptionService.ethGetTransactionByHash(Dao4ArtDexConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[D4aSwapChainServiceImpl] ethGetTransactionByHash result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[D4aSwapChainServiceImpl] ethGetTransactionByHash error result:{}", result.getResultDesc());
            throw new RuntimeException(transactionDto.getTransactionHash() + "cannot find creator");
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[D4aSwapChainServiceImpl] objectMap is null result:{}", result.getData());
            throw new RuntimeException(transactionDto.getTransactionHash() + "cannot find creator");
        }
        String from = (String) objectMap.get("from");
        log.info("[D4aSwapChainServiceImpl] ethGetTransactionByHash creator:{}", from);
        liquidityTransaction.setUserAddress(from.toLowerCase());

        //怎样和D4ASync事件关联起来
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
