package semios.api.service.order;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.web3j.protocol.core.methods.response.Log;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.enums.TradeTypeEnum;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.EventHandleUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

// 创建dao 对事件排序 整理
@Service
@Slf4j
public class CreateDaoEventService {
    @Autowired
    private ISubscriptionService subscribeService;

    /**
     * 监听到 CreateProjectParamEmitted事件(CreateProjectParamEmittedFourChainService.java)后进入处理
     *
     * @param transactionDto 主事件的订阅参数
     */
    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        Result<String> receiptResult = subscribeService.ethGetTransactionReceipt(ProtoDaoConstant.netWork, transactionDto.getTransactionHash());

        if (receiptResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            throw new RuntimeException("subscribeService get transaction is error:" + receiptResult.getResultDesc());
        }

        TransactionReceipt transactionReceipt = JacksonUtil.json2pojo(receiptResult.getData(), TransactionReceipt.class);
        List<Log> logs = transactionReceipt.getLogs();

        // 顺序执行
        // 主事件 不处理，在原订阅中处理
        String type = TradeTypeEnum.CreateProjectParamEmittedFour.getType();
        log.info("TradeTypeEnum.CreateProjectParamEmittedFour main event handleResult is ok:" + JacksonUtil.obj2json(transactionDto.getTransactionHash()));

        boolean handleResult;
        // 补充信息相关
        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.CreateContinuousProjectParamEmitted, transactionDto);  // 扩展信息
        log.info("TradeTypeEnum.CreateContinuousProjectParamEmitted handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.NewProjectForFunding, transactionDto);    // 创建project
        log.info("TradeTypeEnum.NewProjectForFunding handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.NEW_CANVAS, transactionDto);  // 创建canvas
        log.info("TradeTypeEnum.NEW_CANVAS handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.NewPoolsForFunding, transactionDto);  // 创建Pools
        log.info("TradeTypeEnum.NewPoolsForFunding handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.NewSemiDaoErc721Address, transactionDto);
        log.info("TradeTypeEnum.NewSemiDaoErc721Address handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.NewSemiTreasury, transactionDto);
        log.info("TradeTypeEnum.NewSemiTreasury handleResult:" + handleResult);

        // 创建splitter合约
        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.NewD4ARoyaltySplitter, transactionDto);
        log.info("TradeTypeEnum.NewD4ARoyaltySplitter handleResult:" + handleResult);

        // 费率相关
        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.ChildrenSet, transactionDto); // 比例问题
        log.info("TradeTypeEnum.ChildrenSet handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.RatioForFundingSet, transactionDto);
        log.info("TradeTypeEnum.RatioForFundingSet handleResult:" + handleResult);


        // 黑白名单相关
        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.MintCapSet, transactionDto);
        log.info("TradeTypeEnum.MintCapSet handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.MinterBlacklisted, transactionDto); // mint黑名单
        log.info("TradeTypeEnum.MinterBlacklisted handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.CanvasCreatorBlacklisted, transactionDto);   // canvas创建者黑名单
        log.info("TradeTypeEnum.CanvasCreatorBlacklisted handleResult:" + handleResult);

        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.WhitelistModified, transactionDto);
        log.info("TradeTypeEnum.WhitelistModified handleResult:" + handleResult);


        // 关于创建dao的授权,取消掉，，很早的逻辑～确认是否取消
        // 只更新user表,和dao无关
        // handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.ROLE_GRANTED, transactionDto);    // 授权
        // log.info("TradeTypeEnum.ROLE_GRANTED handleResult:"+handleResult);

        // handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.REVOK_EROLE, transactionDto); // 取消授权
        // log.info("TradeTypeEnum.REVOK_EROLE handleResult:"+handleResult);
    }
}
