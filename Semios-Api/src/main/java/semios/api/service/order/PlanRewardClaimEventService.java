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
import semios.api.utils.CommonUtil;
import semios.api.utils.EventHandleUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;
import java.util.Map;

@Service
@Slf4j
public class PlanRewardClaimEventService {
    @Autowired
    private ISubscriptionService subscribeService;

    /**
     * 监听到 PlanRewardClaimSignal事件(PlanRewardClaimSignalChainService.java)后进入处理
     *
     * @param transactionDto 主事件的订阅参数
     */

    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        Result<String> receiptResult = subscribeService.ethGetTransactionReceipt(ProtoDaoConstant.netWork, transactionDto.getTransactionHash());

        // get receipt logs by hash
        if (receiptResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            throw new RuntimeException("subscribeService get transaction is error:" + receiptResult.getResultDesc());
        }
        TransactionReceipt transactionReceipt = JacksonUtil.json2pojo(receiptResult.getData(), TransactionReceipt.class);
        List<Log> logs = transactionReceipt.getLogs();

        // get transaction by hash
        Result<String> resultTransaction = subscribeService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        if (resultTransaction.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[PlanRewardClaimEventService] error result:{}", resultTransaction.getResultDesc());
            throw new RuntimeException("GetTransactionByHash error");
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(resultTransaction.getData());
        if (objectMap == null) {
            log.error("[PlanRewardClaimEventService] objectMap is null result:{}", resultTransaction.getData());
            throw new RuntimeException("GetTransactionByHash objectMap is null");
        }
        String fromAddress = (String) objectMap.get("from");


        // 顺序执行
        // 主事件 不处理，在原订阅中处理
        String type = TradeTypeEnum.PlanRewardClaimSignal.getType();
        log.info("TradeTypeEnum.PlanRewardClaimSignal main event handleResult is ok:" + JacksonUtil.obj2json(transactionDto.getTransactionHash()));

        boolean handleResult;
        // 补充信息相关
        transactionDto.setAddress(fromAddress);    // 设置from为交易发起方
        handleResult = EventHandleUtil.handleEvent(logs, TradeTypeEnum.PlanRewardClaimed, transactionDto);  // 扩展信息
        log.info("TradeTypeEnum.PlanRewardClaimed handleResult:" + handleResult);
    }
}
