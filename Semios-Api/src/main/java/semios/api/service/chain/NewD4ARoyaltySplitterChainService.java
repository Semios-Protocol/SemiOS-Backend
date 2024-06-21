package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.SubscribeRequestDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Subscribe;
import semios.api.model.enums.*;
import semios.api.service.IDaoService;
import semios.api.service.ISubscribeService;
import semios.api.service.SubscriberChainService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.Collections;
import java.util.List;

/**
 * 在创建 DAO 的同一笔交易 hash 中抛出该事件 创建了地址为 addr 的 splitter 合约
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-03-10 13:43
 **/
@Slf4j
@Service
public class NewD4ARoyaltySplitterChainService implements SubscriberChainService {

    // 例子： https://goerli.etherscan.io/tx/0x6682851e5b0c0011ac19b37fc2e6d940004f4e4e1606c256c99fbc185408e4d3#eventlog

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ISubscribeService subscribeService;

    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    /**
     * 监听dao的splitter合约地址
     *
     * @param transactionDto 参数
     * @throws Exception 异常
     */
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewD4ARoyaltySplitterChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String splitterAddr =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(transactionDto.getData())).toLowerCase();

        Dao dao = daoService.selectDaoByTransactionHash(transactionDto.getTransactionHash());
        if (dao == null) {
//            log.warn("[NewD4ARoyaltySplitterChainService] NewD4ARoyaltySplitterChainService cannot find dao");
//            return;
            throw new RuntimeException("NewD4ARoyaltySplitterChainService cannot find dao");
        }
        Subscribe subscribeOld = subscribeService.selectByTradeTypeAndContractTopics(TradeTypeEnum.ETHTransfered.getType(), splitterAddr, ContractMethodEnum.ETH_TRANSFERED.getMethodAddress());
        if (subscribeOld != null) {
            log.info("[NewD4ARoyaltySplitterChainService] subscribeOld is exist transactionDao:{}", JacksonUtil.obj2json(transactionDto));
            return;
        }
        List<Subscribe> subscribeList = subscribeService.selectAll();
        Subscribe subscribe1 =
                subscribeList.stream().filter(v -> TradeTypeEnum.D4A_MINTNFT.getType().equals(v.getTradeType())
                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());

        Subscribe subscribe = new Subscribe();
        subscribe.setContractAddress(splitterAddr);
        subscribe.setTopics(ContractMethodEnum.ETH_TRANSFERED.getMethodAddress());
        subscribe.setFromBlock(transactionDto.getBlockNumber());
        subscribe.setReceiveAddress(subscribe1.getReceiveAddress());
        subscribe.setTradeType(TradeTypeEnum.ETHTransfered.getType());
        subscribe.setOrderInit(subscribeList.size() + 1);
        subscribe.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

        SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
        subscribeRequestDto.setAddress(splitterAddr);
        subscribeRequestDto.setFromBlock(transactionDto.getBlockNumber());
        subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
        subscribeRequestDto.setTopics(Collections.singletonList(ContractMethodEnum.ETH_TRANSFERED.getMethodAddress()));
        subscribeRequestDto.setNoticeType(SubscriberTypeEnum.EVENT.getType());
        subscribeRequestDto.setNoticeUrl(subscribe1.getReceiveAddress());
        subscribeRequestDto.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
        subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
        try {
            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto);
            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
            } else {
                subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                subscribe.setFilterId(subResult.getData());
            }
        } catch (Exception e) {
            log.error("[NewProjectChainService] subscripe erc21 retry error transactionDto:{} e:{}",
                    JacksonUtil.obj2json(transactionDto), e);
            subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setSplitterAddress(splitterAddr);
        int i = daoService.updateDaoAndSubscribe(updateDao, Collections.singletonList(subscribe));
        log.info("[NewD4ARoyaltySplitterChainService] daoId:{} return i:{}", dao.getId(), i);

    }

}
