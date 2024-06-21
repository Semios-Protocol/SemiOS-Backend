package semios.dex.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.request.NoticeSubValueDto;
import semios.dex.model.dto.response.TransactionCallRequestDto;
import semios.dex.model.dto.response.TransactionDto;
import semios.dex.model.entity.Subscribe;
import semios.dex.model.enums.SubscriberStatusEnum;
import semios.dex.model.enums.TradeTypeEnum;
import semios.dex.service.ISubscribeService;
import semios.dex.service.SubscriberChainService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.JacksonUtil;
import semios.dex.utils.SpringBeanUtil;

import java.util.HashSet;
import java.util.Set;

/**
 * 订阅服务相关接口
 *
 * @author xiangbin
 * @ignore 忽略
 */
@Slf4j
@RestController
public class SubscriberController {

    private static final Set<String> stringList = new HashSet<>();
    @Autowired
    private ISubscribeService subscribeService;

    public static void main(String[] args) {
        String a = 1 + "0x92bdd71f8781f6cebf5403bbb5e190c066954a1a7a8eb40e8c4f62ffc43d8700";
        String b = 1 + "0x92bdd71f8781f6cebf5403bbb5e190c066954a1a7a8eb40e8c4f62ffc43d8700";
        stringList.add(a);
        if (stringList.add(a)) {
            log.info("[transaction-call] pelease retry....1");
        }
        stringList.remove(b);

        if (stringList.add(b)) {
            log.info("[transaction-call] pelease retry....2");
        }
    }

    /**
     * transaction回调
     *
     * @ignore 忽略
     */
    @PostMapping(value = "/transaction/call")
    public String transactionCall(@RequestBody(required = false) TransactionCallRequestDto transactionCallResponseDto) {
        log.info("[transactionCall] transactionCallResponseDto:{}", JacksonUtil.obj2json(transactionCallResponseDto));
        if (transactionCallResponseDto == null || transactionCallResponseDto.getResult() == null
                || transactionCallResponseDto.getResult().size() == 0) {
            return SubscriberStatusEnum.SUCCESS.getStatus();
        }
        TransactionDto transactionDto = transactionCallResponseDto.getResult().get(0);
        String stringIndex = transactionDto.getSubId() + transactionDto.getTransactionHash();
        try {
            transactionDto.setBlockTime(transactionCallResponseDto.getTimestamp());
            Subscribe subscribe = subscribeService.selectByFilterId(transactionDto.getSubId().toString());
            if (subscribe == null) {
                // log.error("[transaction-call] subscribe is null subId:{}", transactionDto.getSubId());
                log.info("[transaction-call] subscribe is null transactionCallResponseDto:{}",
                        JacksonUtil.obj2json(transactionCallResponseDto));
                return SubscriberStatusEnum.FAIL.getStatus();
            }
            TradeTypeEnum tradeTypeEnum = TradeTypeEnum.queryByType(subscribe.getTradeType());
            if (tradeTypeEnum == null) {
                log.error("[transaction-call] tradeTypeEnum is null subId:{}", transactionDto.getSubId());
                log.info("[transaction-call] tradeTypeEnum is null transactionCallResponseDto:{}",
                        JacksonUtil.obj2json(transactionCallResponseDto));
                return SubscriberStatusEnum.SUCCESS.getStatus();
            }
            SubscriberChainService subscriberChainService =
                    SpringBeanUtil.getBeanByName(tradeTypeEnum.getTradeServiceName(), SubscriberChainService.class);
            if (subscriberChainService == null) {
                log.error("[transaction-call] subscriberTradeService is null subId:{}", transactionDto.getSubId());
                log.info("[transaction-call] subscriberTradeService is null transactionCallResponseDto:{}",
                        JacksonUtil.obj2json(transactionCallResponseDto));
                return SubscriberStatusEnum.SUCCESS.getStatus();
            }
            transactionDto.setContractAddress(subscribe.getContractAddress());

            if (!stringList.add(stringIndex)) {
                log.warn("[transaction-call] pelease retry stringIndex:{}....", stringIndex);
                return SubscriberStatusEnum.FAIL.getStatus();
            } else {
                log.info("[transaction-call] stringIndex:{} stringList size:{}", stringIndex, stringList.size());
            }
            subscriberChainService.handleTrade(transactionDto);

        } catch (Exception e) {
            log.error("[transaction-call] throw exception transactionCallResponseDto:{}e:",
                    JacksonUtil.obj2json(transactionCallResponseDto), e);
            stringList.remove(stringIndex);
            return SubscriberStatusEnum.FAIL.getStatus();
        }
        stringList.remove(stringIndex);
        return SubscriberStatusEnum.SUCCESS.getStatus();
    }

    /**
     * method回调
     *
     * @ignore 忽略
     */
    @PostMapping(value = "/method/call")
    public String methodCall(@RequestBody(required = false) NoticeSubValueDto noticeSubValueDto) {

        String callValue = CommonUtil.hexToTenString(noticeSubValueDto.getValue());
        log.info("[method-call] noticeSubValueDto:{} CURRENT_ROUND:{}", JacksonUtil.obj2json(noticeSubValueDto),
                callValue);
        if (StringUtils.isBlank(noticeSubValueDto.getSubId())) {
            log.info("[method-call] noticeSubValueDto or subId is null");
            return SubscriberStatusEnum.SUCCESS.getStatus();
        }
        try {

            Subscribe subscribe = subscribeService.selectByFilterId(noticeSubValueDto.getSubId());
            if (subscribe == null) {
                log.info("[method-call] subscribe is null noticeSubValueDto:{}",
                        JacksonUtil.obj2json(noticeSubValueDto));
                return SubscriberStatusEnum.FAIL.getStatus();
            }


        } catch (Exception e) {
            log.error("[method-call] throw exception noticeSubValueDto:{}e:", JacksonUtil.obj2json(noticeSubValueDto),
                    e);
            return SubscriberStatusEnum.FAIL.getStatus();
        }
        return SubscriberStatusEnum.SUCCESS.getStatus();
    }
}
