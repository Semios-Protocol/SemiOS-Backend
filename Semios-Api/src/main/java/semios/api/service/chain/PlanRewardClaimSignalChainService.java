package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.service.SubscriberChainService;
import semios.api.service.order.PlanRewardClaimEventService;
import semios.api.utils.JacksonUtil;

@Slf4j
@Service
public class PlanRewardClaimSignalChainService implements SubscriberChainService {

    @Autowired
    private PlanRewardClaimEventService planRewardClaimEventService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        // 信号事件，直接处理
        log.info("[PlanRewardClaimSignalChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        planRewardClaimEventService.handleTrade(transactionDto);
    }
}
