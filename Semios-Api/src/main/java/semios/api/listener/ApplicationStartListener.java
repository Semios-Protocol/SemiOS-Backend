package semios.api.listener;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Configuration;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.SubscribeRequestDto;
import semios.api.model.entity.ShutdownRecord;
import semios.api.model.entity.Subscribe;
import semios.api.model.enums.ShutdownTypeEnum;
import semios.api.model.enums.SubscribeStatusEnum;
import semios.api.model.enums.TradeTypeEnum;
import semios.api.service.IDaoService;
import semios.api.service.IShutdownRecordService;
import semios.api.service.ISubscribeService;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.ProtoDaoCommonUtil;

import java.util.Collections;
import java.util.List;

/**
 * @description: listener
 * @author: xiangbin
 * @create: 2022-04-22 16:10
 **/
@Slf4j
@Configuration
public class ApplicationStartListener implements CommandLineRunner {

    @Autowired
    private ISubscribeService subscribeService;

    @Autowired
    private IDaoService daoService;

    @Autowired(required = false)
    private ISubscriptionService subscriptionService;

    @Autowired
    private IShutdownRecordService shutdownRecordService;

    @Autowired
    private CommonService commonService;

    @Override
    public void run(String... args) throws Exception {
        try {
            log.info("ApplicationStartListener running start");
            List<Subscribe> subscribeList = subscribeService.selectAll();
            for (Subscribe subscribe : subscribeList) {
                TradeTypeEnum tradeTypeEnum = TradeTypeEnum.queryByType(subscribe.getTradeType());
                if (tradeTypeEnum == null) {
                    log.error("[ApplicationStartListener]tradeTypeEnum is null subId:{}", subscribe.getId());
                    continue;
                }
                if (tradeTypeEnum.getLocal()) {
                    // 本地存储的值
                    String value = ProtoDaoCommonUtil.ethCall(subscriptionService, ProtoDaoConstant.netWork,
                            subscribe.getContractAddress(), subscribe.getTopics());
                    log.info("ethcall return subscribe:{}, value:{}", subscribe.getTradeType(), value);
                    if (StringUtils.isBlank(value)) {
                        log.error("[ApplicationStartListener]tradeTypeEnum value is null subId:{}", subscribe.getId());
                        continue;
                    }

                    TradeTypeEnum.setDefaultValue(tradeTypeEnum, value);

                } else {
                    String fromBlock = subscribe.getFromBlock();
                    // 需要订阅的值
                    if (StringUtils.isNotBlank(subscribe.getFilterId())) {
                        Result<String> result = subscriptionService.subscripeStatus(subscribe.getFilterId());
                        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                            log.error(
                                    "[ApplicationStartListener] subscripeStatus error filterId:{} status resultDesc:{}",
                                    subscribe.getFilterId(), result.getResultDesc());
                            continue;
                        }
                        if (result.getData().equalsIgnoreCase(String.valueOf(SubscribeStatusEnum.OPEN.getType()))) {// 0-关闭
                            // 1-开启
                            // 开启则不用管，如果已经关闭了，则查询上次通知的区块，然后重新订阅
                            log.info("[ApplicationStartListener] subId:{} status is ok!", subscribe.getId());
                            continue;
                        }
                    }

                    SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
                    subscribeRequestDto.setAddress(subscribe.getContractAddress());
                    subscribeRequestDto.setFromBlock(fromBlock);
                    subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
                    subscribeRequestDto.setTopics(Collections.singletonList(subscribe.getTopics()));
                    subscribeRequestDto.setNoticeType(tradeTypeEnum.getSubscriberTypeEnum().getType());
                    subscribeRequestDto.setNoticeUrl(subscribe.getReceiveAddress());
                    if (subscribe.getIntervalTime() != null) {
                        subscribeRequestDto.setIntervalPeriod(subscribe.getIntervalTime());
                    }
                    subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + ProtoDaoConstant.appName);
                    Result<String> result = subscriptionService.subscripe(subscribeRequestDto);
                    if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                        log.error("[ApplicationStartListener] subscripe retry error filterId:{} status resultDesc:{}",
                                subscribe.getFilterId(), result.getResultDesc());
                        subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
                    } else {
                        subscribe.setFilterId(result.getData());
                        subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    }
                    subscribeService.updateById(subscribe);

                }
            }

            // TODO 对current_round写死
            // 原来对版本是合约统一周期，现在每个dao都有自己对周期，而且，这个合约已经删除
            ProtoDaoConstant.CURRENT_ROUND = "1";
            //赋值给Dao4ArtConstant.NEXT_DRB_START_BLOCK
            if (StringUtils.isNotBlank(ProtoDaoConstant.CURRENT_ROUND)) {
                // TODO 写死的值，待确认
                ProtoDaoConstant.NEXT_DRB_START_BLOCK = Integer.valueOf("1000000");
                //commonService.handleNextDrbStartBlock(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
            }


            ShutdownRecord shutdownRecord = shutdownRecordService.selectByType(ShutdownTypeEnum.D4A.getType(), null);
            if (shutdownRecord != null && shutdownRecord.getIsPaused() == 0) {
                ProtoDaoConstant.D4APause = true;
            }

            log.info("ApplicationStartListener running end");

        } catch (Exception e) {
            log.error("[ApplicationStartListener] logs only for exception e:", e);
            throw new Exception("start error!");
        }
    }

}
