package semios.dex.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.dex.model.dto.common.Dao4ArtDexConstant;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.response.TransactionDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.enums.LiquidityTradeTypeEnum;
import semios.dex.model.enums.OneOrZeroEnum;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ILiquidityTransactionService;
import semios.dex.service.SubscriberChainService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * 二次交易
 *
 * @description: 二次交易
 * @author: xiangbin
 * @create: 2023-05-17
 **/
@Slf4j
@Service
@Deprecated
public class EthTransferedChainServiceImpl implements SubscriberChainService {

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ISubscriptionService subscriptionService;

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[D4ABurnChainServiceImpl] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String amount0 = CommonUtil.hexToTenString(dataList.get(0));
        String amount1 = CommonUtil.hexToTenString(dataList.get(1));

        log.info("[D4ABurnChainServiceImpl] transactionHash:{} amount0:{} amount1:{} pairAddress:{}", transactionDto.getTransactionHash(), amount0, amount1, transactionDto.getContractAddress());

        if (amount0 == null || amount1 == null || StringUtils.isAnyBlank(amount0, amount1)) {
            throw new RuntimeException(transactionDto.getContractAddress() + " is Illegal transaction");
        }

        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByPairAddress(transactionDto.getContractAddress());
        if (erc20Liquidity == null) {
            throw new RuntimeException(transactionDto.getContractAddress() + " Erc20Liquidity not find");
        }

        LiquidityTransaction liquidityTransaction = new LiquidityTransaction();
        liquidityTransaction.setErc20Address(erc20Liquidity.getErc20Address());
        liquidityTransaction.setErc20Name(erc20Liquidity.getErc20Name());
        liquidityTransaction.setErc20Symbol(erc20Liquidity.getErc20Symbol());
        liquidityTransaction.setTradeType(LiquidityTradeTypeEnum.REMOVE.getStatus());
        liquidityTransaction.setTransactionHash(transactionDto.getTransactionHash());
        liquidityTransaction.setBlockTime(Long.valueOf(transactionDto.getBlockTime()));
        if (erc20Liquidity.getErc20Order().equals(OneOrZeroEnum.ZERO.getStatus())) {
            liquidityTransaction.setInTokenAmount(new BigDecimal(amount1));
            liquidityTransaction.setOutTokenAmount(new BigDecimal(amount0));
            liquidityTransaction.setEthAmount(new BigDecimal(amount1));
        } else {
            liquidityTransaction.setInTokenAmount(new BigDecimal(amount0));
            liquidityTransaction.setOutTokenAmount(new BigDecimal(amount1));
            liquidityTransaction.setEthAmount(new BigDecimal(amount0));
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

}
