package semios.dex.listener;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Configuration;
import semios.dex.model.dto.common.Dao4ArtDexConstant;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.request.SubscribeRequestDto;
import semios.dex.model.entity.Subscribe;
import semios.dex.model.enums.SubscribeStatusEnum;
import semios.dex.model.enums.TradeTypeEnum;
import semios.dex.service.ISubscribeService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.ProtoDaoDexCommonUtil;

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
    private ISubscriptionService subscriptionService;


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
                    //本地存储的值
                    String value = ProtoDaoDexCommonUtil.ethCall(subscriptionService, Dao4ArtDexConstant.netWork, subscribe.getContractAddress(), subscribe.getTopics());
                    log.info("ethcall return subscribe:{}, value:{}", subscribe.getTradeType(), value);
                    if (StringUtils.isBlank(value)) {
                        log.error("[ApplicationStartListener]tradeTypeEnum value is null subId:{}", subscribe.getId());
                        throw new RuntimeException("local value cannot find");
                    }

                    TradeTypeEnum.setDefaultValue(tradeTypeEnum, value);

                    subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribeService.updateById(subscribe);

                } else {
                    String fromBlock = subscribe.getFromBlock();
                    //需要订阅的值
                    if (StringUtils.isNotBlank(subscribe.getFilterId())) {
                        Result<String> result = subscriptionService.subscripeStatus(subscribe.getFilterId());
                        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                            log.error("[ApplicationStartListener] subscripeStatus error filterId:{} status resultDesc:{}", subscribe.getFilterId(), result.getResultDesc());
                            continue;
                        }
                        //0-关闭 1-开启  开启则不用管，如果已经关闭了，则查询上次通知的区块，然后重新订阅
                        if (result.getData().equalsIgnoreCase(String.valueOf(SubscribeStatusEnum.OPEN.getType()))) {
                            log.info("[ApplicationStartListener] subId:{} status is ok!", subscribe.getId());
                            continue;
                        }
                    }

                    SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
                    subscribeRequestDto.setAddress(subscribe.getContractAddress());
                    subscribeRequestDto.setFromBlock(fromBlock);
                    subscribeRequestDto.setNetwork(Dao4ArtDexConstant.netWork);
                    subscribeRequestDto.setTopics(Collections.singletonList(subscribe.getTopics()));
                    subscribeRequestDto.setNoticeType(tradeTypeEnum.getSubscriberTypeEnum().getType());
                    subscribeRequestDto.setNoticeUrl(subscribe.getReceiveAddress());
                    subscribeRequestDto.setAppName(Dao4ArtDexConstant.netWork + "-" + "dao4artDex");
                    Result<String> result = subscriptionService.subscripe(subscribeRequestDto);
                    if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                        log.error("[ApplicationStartListener] subscripe retry error filterId:{} status resultDesc:{}", subscribe.getFilterId(), result.getResultDesc());
                        subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
                    } else {
                        subscribe.setFilterId(result.getData());
                        subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    }
                    subscribeService.updateById(subscribe);
                }
            }


            log.info("ApplicationStartListener running end");

        } catch (Exception e) {
            log.error("[ApplicationStartListener] logs only for exception e:", e);
            throw new Exception("start error!");
        }
    }


}
