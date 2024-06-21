package semios.api.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.task.TaskExecutor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.request.NoticeSubValueDto;
import semios.api.model.dto.request.TransactionCallRequestDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Subscribe;
import semios.api.model.enums.ContractMethodEnum;
import semios.api.model.enums.SubscriberStatusEnum;
import semios.api.model.enums.SubscriberTypeEnum;
import semios.api.model.enums.TradeTypeEnum;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.HashSet;
import java.util.List;
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
    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private ICanvasService canvasService;
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;
    @Autowired
    private CommonService commonService;
    @Autowired
    private TaskExecutor taskExecutor;

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
                return SubscriberStatusEnum.FAIL.getStatus();
            }
            transactionDto.setContractAddress(subscribe.getContractAddress());
            transactionCallResponseDto.getResult().forEach(v -> v.setContractAddress(subscribe.getContractAddress()));
            if (!stringList.add(stringIndex)) {
                log.warn("[transaction-call] pelease retry stringIndex:{}....", stringIndex);
                return SubscriberStatusEnum.FAIL.getStatus();
            } else {
                log.info("[transaction-call] stringIndex:{} stringList size:{}", stringIndex, stringList.size());
            }
            SubscriberTypeEnum subscriberTypeEnum = tradeTypeEnum.getSubscriberTypeEnum();
            if (SubscriberTypeEnum.BATCH_TRAN.getType().equals(subscriberTypeEnum.getType())) {
                subscriberChainService.handleTradeList(transactionCallResponseDto.getResult());
            } else {
                subscriberChainService.handleTrade(transactionDto);
            }

        } catch (Exception e) {
            log.error("[transaction-call] throw exception transactionCallResponseDto:{}e:",
                    JacksonUtil.obj2json(transactionCallResponseDto), e);
            stringList.remove(stringIndex);
            return SubscriberStatusEnum.FAIL.getStatus();
        }
        //不在finally中删除因为怕删除了其他线程加入的key
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
            TradeTypeEnum tradeTypeEnum = TradeTypeEnum.queryByType(subscribe.getTradeType());
            if (tradeTypeEnum == null) {
                log.error("[transaction-call] tradeTypeEnum is null subId:{}", noticeSubValueDto.getSubId());
                log.info("[transaction-call] tradeTypeEnum is null noticeSubValueDto:{}",
                        JacksonUtil.obj2json(noticeSubValueDto));
                return SubscriberStatusEnum.SUCCESS.getStatus();
            }
            SubscriberChainService subscriberChainService =
                    SpringBeanUtil.getBeanByName(tradeTypeEnum.getTradeServiceName(), SubscriberChainService.class);
            if (subscriberChainService == null) {
                log.error("[transaction-call] subscriberTradeService is null subId:{}", noticeSubValueDto.getSubId());
                log.info("[transaction-call] subscriberTradeService is null noticeSubValueDto:{}",
                        JacksonUtil.obj2json(noticeSubValueDto));
                return SubscriberStatusEnum.FAIL.getStatus();
            }

            noticeSubValueDto.setTopics(subscribe.getTopics());
            subscriberChainService.handleTradeValue(noticeSubValueDto);


            /** 1.4 不需要drb这个概念了。
             if (StringUtils.isNotBlank(callValue) && StringUtils.isNotBlank(ProtoDaoConstant.CURRENT_ROUND) && !callValue.equals(ProtoDaoConstant.CURRENT_ROUND)) {
             Integer currentRount = Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND);
             ProtoDaoConstant.CURRENT_ROUND = callValue;
             // 做drb结算，变更drb
             //                SearchController.searchExecutor.execute(() -> commonService.currentDrbDaoStatistics(currentRount));
             taskExecutor.execute(() -> commonService.currentDrbDaoStatistics(currentRount));
             commonService.handleNextDrbStartBlock(Integer.valueOf(callValue));
             commonService.updateTopupModeMinterHarvest(currentRount);
             // SearchController.searchExecutor.execute(() ->
             // currentDrbCanvasStatistics(Integer.valueOf(Dao4ArtConstant.CURRENT_ROUND)));
             // 做dao的开始和关闭操作
             //                if (StringUtils.isNotBlank(callValue)) {
             //                    SearchController.searchExecutor.execute(() -> commonService.startDao(Integer.valueOf(callValue)));
             //
             //                    SearchController.searchExecutor.execute(() -> commonService.createCurrentDrbDaoStatistics(Integer.valueOf(callValue)));
             //                }
             // SearchController.searchExecutor.execute(() -> closeDao(Integer.valueOf(callValue)));


             }
             **/

        } catch (Exception e) {
            log.error("[method-call] throw exception noticeSubValueDto:{}e:", JacksonUtil.obj2json(noticeSubValueDto),
                    e);
            return SubscriberStatusEnum.FAIL.getStatus();
        }
        return SubscriberStatusEnum.SUCCESS.getStatus();
    }

    /**
     * 更新dao下所有的可领取代币数量
     */
    public void updateDaoToken() {

        List<Dao> daoList = daoService.daoStarted();
        for (Dao dao : daoList) {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.PROJECT_CLAIM.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.PROJECT_CLAIM.getMethodAddress() + dao.getProjectId());

            // 调用查询使用数据集的user
            Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[updateDaoToken] error result:{} daoId:{}", result.getResultDesc(), dao.getId());
                // throw new RuntimeException("更新dao代币数量异常");
                continue;
            }
            log.info("[NewCanvasChainService]infura return data:{}", result.getData());
            String canvasInfoData = result.getData();
            String price = CommonUtil.hexToTenString(canvasInfoData);
            if (StringUtils.isNotBlank(price) && !"0".equals(price)) {
                Dao updateDao = new Dao();
                updateDao.setId(dao.getId());
                updateDao.setUnclaimedToken(
                        new BigDecimal(price).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR));
                daoService.updateById(updateDao);
            }
        }
    }

    /**
     * 更新dao下所有的可领取代币数量
     */
    public void updateCanvasToken(Integer drbNumber) {

        List<Canvas> canvasList = canvasService.listCanvasLessThanDrb(drbNumber);
        for (Canvas canvas : canvasList) {
            // canvas next price
            Dao dao = daoService.getById(canvas.getDaoId());
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.CLAIM_CANVAS_REWARD.getContractAddress());
            infuraCallRequestDto
                    .setData(ContractMethodEnum.CLAIM_CANVAS_REWARD.getMethodAddress() + canvas.getCanvasId());

            // 调用查询使用数据集的user
            Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[updateCanvasToken] error result:{} canvasId:{}", result.getResultDesc(), canvas.getId());
                // throw new RuntimeException("保存canvas查询价格信息失败");
                continue;
            }
            log.info("[updateCanvasToken]infura return data:{}", result.getData());
            String canvasInfoData = result.getData();
            String price = CommonUtil.hexToTenString(canvasInfoData);
            if (StringUtils.isNotBlank(price) && !"0".equals(price)) {
                BigDecimal token =
                        new BigDecimal(price).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                canvas.setUnclaimedToken(token);
                CanvasDrbStatistics canvasDrbStatistics =
                        canvasDrbStatisticsService.selectByCanvasIdAndDrbNumber(canvas.getId(), drbNumber);
                BigDecimal receivedToken = canvas.getReceivedToken();
                if (receivedToken == null) {
                    receivedToken = BigDecimal.ZERO;
                }
                canvasDrbStatistics.setDaoReward(token.add(receivedToken));
                canvasDrbStatisticsService.updateById(canvasDrbStatistics);
                canvasService.updateById(canvas);
            }

        }
    }
}
