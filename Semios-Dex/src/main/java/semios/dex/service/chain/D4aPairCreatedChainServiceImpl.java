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
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ISubscribeService;
import semios.dex.service.SubscriberChainService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.JacksonUtil;

import java.util.List;
import java.util.Map;

/**
 * 创建交易对
 *
 * @description: 创建交易对
 * @author: xiangbin
 * @create: 2023-05-17
 **/
@Slf4j
@Service
public class D4aPairCreatedChainServiceImpl implements SubscriberChainService {

    //示例： https://goerli.etherscan.io/tx/0xb6d1521c5fc85d5aa89821c8007137007ceca1b841a71a27ef95ac1b5881e070#eventlog


    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ISubscriptionService subscriptionService;

    @Autowired
    private ISubscribeService subscribeService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[D4APairCreatedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        //确定token0和token1的顺序
        String firstAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        String secondAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String pairAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(0))).toLowerCase();

        String erc20Address, ethAddress;
        if (StringUtils.isBlank(Dao4ArtDexConstant.WETH_ADDRESS)) {
            log.error("[D4APairCreatedChainService] weth address is null transactionHash:{} ", transactionDto.getTransactionHash());
            throw new RuntimeException("wethAddress is null");
        }
        log.info("[D4APairCreatedChainService] firstAddress:{} secondAddress:{}", firstAddress, secondAddress);
        if (firstAddress.equalsIgnoreCase(Dao4ArtDexConstant.WETH_ADDRESS)) {
            erc20Address = secondAddress;
            ethAddress = firstAddress;
        } else {
            erc20Address = firstAddress;
            ethAddress = secondAddress;
        }
        log.info("[D4APairCreatedChainService] transactionHash:{} erc20Address:{} ethAddress:{} pairAddress:{}", transactionDto.getTransactionHash(), erc20Address, ethAddress, pairAddress);

        //判断哪个是erc20地址，哪个是eth地址
        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20Address);
        if (erc20Liquidity == null) {
            log.warn("[D4APairCreatedChainService] erc20 not exist transactionHash:{} erc20Address:{} ethAddress:{} pairAddress:{}", transactionDto.getTransactionHash(), erc20Address, ethAddress, pairAddress);
            erc20Liquidity = new Erc20Liquidity();
        }
        if (StringUtils.isNotBlank(erc20Liquidity.getPairAddress()) && erc20Liquidity.getPairAddress().equals(pairAddress)) {
            log.warn("[D4APairCreatedChainService] exist transactionHash:{} erc20Address:{} ethAddress:{} pairAddress:{}", transactionDto.getTransactionHash(), erc20Address, ethAddress, pairAddress);
            return;
        }
        erc20Liquidity.setErc20Address(erc20Address);
        erc20Liquidity.setEthAddress(ethAddress);
        erc20Liquidity.setPairAddress(pairAddress);
        erc20Liquidity.setTransactionHash(transactionDto.getTransactionHash());
        erc20Liquidity.setBlockTime(Long.valueOf(transactionDto.getBlockTime()));
        // 查询创建者
        Result<String> result = subscriptionService.ethGetTransactionByHash(Dao4ArtDexConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[D4APairCreatedChainService] ethGetTransactionByHash result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[D4APairCreatedChainService] ethGetTransactionByHash error result:{}", result.getResultDesc());
            throw new RuntimeException(transactionDto.getTransactionHash() + "cannot find creator");
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[D4APairCreatedChainService] objectMap is null result:{}", result.getData());
            throw new RuntimeException(transactionDto.getTransactionHash() + "cannot find creator");
        }
        String from = (String) objectMap.get("from");
        log.info("[D4APairCreatedChainService] ethGetTransactionByHash creator:{}", from);
        erc20Liquidity.setCreateAddress(from.toLowerCase());
        erc20Liquidity.setErc20Order(0);
        if (firstAddress.equalsIgnoreCase(Dao4ArtDexConstant.WETH_ADDRESS)) {
            erc20Liquidity.setErc20Order(1);
        }

        erc20LiquidityService.saveOrUpdate(erc20Liquidity);

//        List<Subscribe> subscribeSaveList = new ArrayList<>();
//        List<Subscribe> subscribeList = subscribeService.selectAll();
//        Subscribe subscribePair =
//                subscribeList.stream().filter(v -> TradeTypeEnum.D4APairCreated.getType().equals(v.getTradeType())
//                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());
//
//        Subscribe subscribe = new Subscribe();
//        subscribe.setContractAddress(pairAddress);
//        subscribe.setTopics(ContractMethodEnum.D4ATransfer.getMethodAddress());
//        subscribe.setFromBlock(subscribePair.getFromBlock());
//        subscribe.setReceiveAddress(subscribePair.getReceiveAddress());
//        subscribe.setTradeType(TradeTypeEnum.D4ATransfer.getType());
//        subscribe.setOrderInit(subscribeList.size() + 1);
//
//        SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
//        subscribeRequestDto.setAddress(pairAddress);
//        subscribeRequestDto.setFromBlock(subscribePair.getFromBlock());
//        subscribeRequestDto.setNetwork(Dao4ArtDexConstant.netWork);
//        subscribeRequestDto.setTopics(Collections.singletonList(ContractMethodEnum.D4ATransfer.getMethodAddress()));
//        subscribeRequestDto.setNoticeType(SubscriberTypeEnum.EVENT.getType());
//        subscribeRequestDto.setNoticeUrl(subscribePair.getReceiveAddress());
//        subscribeRequestDto.setAppName(Dao4ArtDexConstant.netWork + "-" + "dao4artDex");
//        try {
//            Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//        subscribeSaveList.add(subscribe);
//
//
//        Subscribe subscribe1 = new Subscribe();
//        subscribe1.setContractAddress(pairAddress);
//        subscribe1.setTopics(ContractMethodEnum.D4AMint.getMethodAddress());
//        subscribe1.setFromBlock(subscribePair.getFromBlock());
//        subscribe1.setReceiveAddress(subscribePair.getReceiveAddress());
//        subscribe1.setTradeType(TradeTypeEnum.D4AMint.getType());
//        subscribe1.setOrderInit(subscribeList.size() + 1);
//
//        SubscribeRequestDto subscribeRequestDto1 = new SubscribeRequestDto();
//        subscribeRequestDto1.setAddress(pairAddress);
//        subscribeRequestDto1.setFromBlock(subscribePair.getFromBlock());
//        subscribeRequestDto1.setNetwork(Dao4ArtDexConstant.netWork);
//        subscribeRequestDto1.setTopics(Collections.singletonList(ContractMethodEnum.D4AMint.getMethodAddress()));
//        subscribeRequestDto1.setNoticeType(SubscriberTypeEnum.EVENT.getType());
//        subscribeRequestDto1.setNoticeUrl(subscribePair.getReceiveAddress());
//        subscribeRequestDto1.setAppName(Dao4ArtDexConstant.netWork + "-" + "dao4art");
//        try {
//            Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto1);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe1.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe1.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe1.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe1.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//        subscribeSaveList.add(subscribe1);
//
//        Subscribe subscribe2 = new Subscribe();
//        subscribe2.setContractAddress(pairAddress);
//        subscribe2.setTopics(ContractMethodEnum.D4ASync.getMethodAddress());
//        subscribe2.setFromBlock(subscribePair.getFromBlock());
//        subscribe2.setReceiveAddress(subscribePair.getReceiveAddress());
//        subscribe2.setTradeType(TradeTypeEnum.D4ASync.getType());
//        subscribe2.setOrderInit(subscribeList.size() + 1);
//
//        SubscribeRequestDto subscribeRequestDto2 = new SubscribeRequestDto();
//        subscribeRequestDto2.setAddress(pairAddress);
//        subscribeRequestDto2.setFromBlock(subscribePair.getFromBlock());
//        subscribeRequestDto2.setNetwork(Dao4ArtDexConstant.netWork);
//        subscribeRequestDto2.setTopics(Collections.singletonList(ContractMethodEnum.D4ASync.getMethodAddress()));
//        subscribeRequestDto2.setNoticeType(SubscriberTypeEnum.EVENT.getType());
//        subscribeRequestDto2.setNoticeUrl(subscribePair.getReceiveAddress());
//        subscribeRequestDto2.setAppName(Dao4ArtDexConstant.netWork + "-" + "dao4art");
//        try {
//            Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto2);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe2.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe2.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//        subscribeSaveList.add(subscribe2);
//
//        Subscribe subscribe3 = new Subscribe();
//        subscribe3.setContractAddress(pairAddress);
//        subscribe3.setTopics(ContractMethodEnum.D4ABurn.getMethodAddress());
//        subscribe3.setFromBlock(subscribePair.getFromBlock());
//        subscribe3.setReceiveAddress(subscribePair.getReceiveAddress());
//        subscribe3.setTradeType(TradeTypeEnum.D4ABurn.getType());
//        subscribe3.setOrderInit(subscribeList.size() + 1);
//
//        SubscribeRequestDto subscribeRequestDto3 = new SubscribeRequestDto();
//        subscribeRequestDto3.setAddress(pairAddress);
//        subscribeRequestDto3.setFromBlock(subscribePair.getFromBlock());
//        subscribeRequestDto3.setNetwork(Dao4ArtDexConstant.netWork);
//        subscribeRequestDto3.setTopics(Collections.singletonList(ContractMethodEnum.D4ABurn.getMethodAddress()));
//        subscribeRequestDto3.setNoticeType(SubscriberTypeEnum.EVENT.getType());
//        subscribeRequestDto3.setNoticeUrl(subscribePair.getReceiveAddress());
//        subscribeRequestDto3.setAppName(Dao4ArtDexConstant.netWork + "-" + "dao4art");
//        try {
//            Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto3);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe3.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe3.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe3.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe3.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//
//        subscribeSaveList.add(subscribe3);
//
//        Subscribe subscribe4 = new Subscribe();
//        subscribe4.setContractAddress(pairAddress);
//        subscribe4.setTopics(ContractMethodEnum.D4ASwap.getMethodAddress());
//        subscribe4.setFromBlock(subscribePair.getFromBlock());
//        subscribe4.setReceiveAddress(subscribePair.getReceiveAddress());
//        subscribe4.setTradeType(TradeTypeEnum.D4ASwap.getType());
//        subscribe4.setOrderInit(subscribeList.size() + 1);
//
//        SubscribeRequestDto subscribeRequestDto4 = new SubscribeRequestDto();
//        subscribeRequestDto4.setAddress(pairAddress);
//        subscribeRequestDto4.setFromBlock(subscribePair.getFromBlock());
//        subscribeRequestDto4.setNetwork(Dao4ArtDexConstant.netWork);
//        subscribeRequestDto4.setTopics(Collections.singletonList(ContractMethodEnum.D4ASwap.getMethodAddress()));
//        subscribeRequestDto4.setNoticeType(SubscriberTypeEnum.EVENT.getType());
//        subscribeRequestDto4.setNoticeUrl(subscribePair.getReceiveAddress());
//        subscribeRequestDto4.setAppName(Dao4ArtDexConstant.netWork + "-" + "dao4art");
//        try {
//            Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto4);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe4.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe4.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe4.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe4.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//        subscribeSaveList.add(subscribe4);


//        erc20LiquidityService.saveErc20LiquidityAndSubscribe(erc20Liquidity, subscribeSaveList);


    }

}
