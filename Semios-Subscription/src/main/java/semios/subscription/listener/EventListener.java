package semios.subscription.listener;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestTemplate;
import semios.subscription.model.dto.CallParams;
import semios.subscription.model.dto.NoticeSubValueDto;
import semios.subscription.model.dto.request.InfuraParamsForFilterDto;
import semios.subscription.model.dto.response.InfuraResponseDto;
import semios.subscription.model.dto.response.InfuraResponseTransactionListDto;
import semios.subscription.model.dto.response.InfuraTransactionDto;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.model.entity.SubNumValue;
import semios.subscription.model.entity.Subscriber;
import semios.subscription.model.entity.Transaction;
import semios.subscription.model.enums.NetWorkEnum;
import semios.subscription.model.enums.NoticeTypeEnum;
import semios.subscription.model.enums.SubStatusEnum;
import semios.subscription.schedule.SyncTransactionSchedule;
import semios.subscription.service.IBlockHeightService;
import semios.subscription.service.ISubNumValueService;
import semios.subscription.service.ISubscriberService;
import semios.subscription.service.ITransactionService;
import semios.subscription.utils.CommonUtil;
import semios.subscription.utils.InfuraMetodUtils;
import semios.subscription.utils.JacksonUtil;
import semios.subscription.utils.SpringBeanUtil;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @description: listener
 * @author: xiangbin
 * @create: 2022-04-13 15:01
 **/
@Slf4j
@Configuration
public class EventListener implements CommandLineRunner {

    public static String infuraProjectId;
    @Autowired
    private ISubscriberService subscriberService;
    @Autowired
    private ITransactionService transactionService;
    @Autowired
    private IBlockHeightService blockHeightService;
    @Autowired
    private ISubNumValueService subNumValueService;

    public static void main(String[] args) {
        List<Integer> ids = new ArrayList<>();
        ids.add(1);
        ids.add(2);
        ids.add(3);
        ids.add(4);
        ids.add(5);
        ids = ids.stream().sorted(Comparator.comparing(v -> v)).collect(Collectors.toList());
        for (Integer id : ids) {
            System.out.println(id);
        }
    }

    @Value("${infura_projectId}")
    public void setInfuraProjectId(String infuraProjectId) {
        log.info("infuraProjectId:{}", infuraProjectId);
        EventListener.infuraProjectId = infuraProjectId;
    }

    @Override
    public void run(String[] args) {
        log.info("[EventListener]start..");
        ThreadPoolUtils.scheduler.scheduleAtFixedRate(() -> {
//            log.info("[EventListener]running..");
            log.info("[EventListener1]start..");
            try {
                List<Subscriber> subscribers = subscriberService.findAllOpenTenSub();
                if (subscribers.size() == 0) {
                    return;
                }
                //只有区块的高度变化了，才触发获取logs的事件和触发取值事件
                Map<String, List<Subscriber>> subMap = subscribers.stream().collect(Collectors.groupingBy(Subscriber::getNetwork));
                for (String str : subMap.keySet()) {
                    NetWorkEnum netWorkEnum = NetWorkEnum.getByName(str);
                    if (netWorkEnum == null) {
                        log.error("[EventListener]netWorkEnum is null str:{}", str);
                        continue;
                    }
                    BlockHeight blockHeight = blockHeightService.getBySubId(netWorkEnum.getId());
                    if (blockHeight == null) {
                        blockHeight = new BlockHeight();
                        blockHeight.setModifyTime(LocalDateTime.now());
                        blockHeight.setSubId(Integer.valueOf(netWorkEnum.getId()));
                    }
                    String filterId;
                    if (StringUtils.isBlank(blockHeight.getFilterId()) || blockHeight.getModifyTime() == null || blockHeight.getModifyTime().isBefore(LocalDateTime.now().minus(15, ChronoUnit.MINUTES))) {
                        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethNewBlockFilter(str);
                        filterId = infuraResponseDto.getResult();
                        blockHeight.setFilterId(filterId);
                        blockHeight.setModifyTime(LocalDateTime.now());
                        blockHeightService.saveOrUpdate(blockHeight);
                    }
                    filterId = blockHeight.getFilterId();


                    InfuraResponseTransactionListDto infuraResponseTransactionListDto = InfuraMetodUtils.ethGetFilterChanges(str, filterId);

                    if (infuraResponseTransactionListDto == null || infuraResponseTransactionListDto.getResult() == null) {
                        log.info("infuraResponseTransactionListDto is null");
                        continue;
                    }
                    if (infuraResponseTransactionListDto.getResult().size() > 0) {

                        blockHeight.setModifyTime(LocalDateTime.now());
                        blockHeightService.saveOrUpdate(blockHeight);

                        Map<String, String> allMap = new HashMap<>();

                        List<Subscriber> subscriberList = subMap.get(str);
                        List<Subscriber> subscriberListForTran = subscriberList.stream().filter(v -> NoticeTypeEnum.TRAN.getType().equals(v.getNoticeType()) || NoticeTypeEnum.BATCH_TRAN.getType().equals(v.getNoticeType())).collect(Collectors.toList());
                        List<Subscriber> subscriberListForNum = subscriberList.stream().filter(v -> NoticeTypeEnum.NUM.getType().equals(v.getNoticeType())).collect(Collectors.toList());
                        log.info("subscriberListForTran size:{}", subscriberListForTran.size());
                        log.info("subscriberListForNum size:{}", subscriberListForNum.size());

                        int count = 0;
                        if (subscriberListForTran.size() > 0) {
                            count++;
                        }
                        if (subscriberListForNum.size() > 0) {
                            count++;
                        }
                        CountDownLatch countDownLatch = new CountDownLatch(count);
//                        log.info("[EventListener] countDownLatch countDownLatch size:{}", count);
                        if (subscriberListForTran.size() > 0) {
                            ThreadPoolUtils.filterExecutor.execute(() -> subscriberForTran(subscriberListForTran, allMap, countDownLatch));
                        }
                        if (subscriberListForNum.size() > 0) {
                            ThreadPoolUtils.filterExecutor.execute(() -> subscriberForNum(subscriberListForNum, allMap, countDownLatch));

                        }

                        countDownLatch.await();
//                        log.info("[EventListener] countDownLatch exec");
                        if (allMap.keySet().size() > 0) {
                            String lineSeparator = System.getProperty("line.separator");
                            StringJoiner stringJoiner = new StringJoiner("|" + lineSeparator);
                            for (String key : allMap.keySet()) {
                                stringJoiner.add(key + ":" + allMap.get(key));
                            }
                            log.info("[EventListener] blockHeigh:{} values:{}", allMap.get("blockNo"), stringJoiner);
                        }

                    }
                }

            } catch (ResourceAccessException e) {
                log.warn("EventListener Exception error e:", e);
            } catch (Exception e) {
                log.error("EventListener Exception error e:", e);
            }
            log.info("[EventListener-1]end..");

        }, 3, 10, TimeUnit.SECONDS);


        log.info("[EventListener] sixty start..");
        ThreadPoolUtils.scheduler.scheduleAtFixedRate(() -> {
//            log.info("[EventListener]running..");
            log.info("[EventListener-2]start..");
            try {
                List<Subscriber> subscribers = subscriberService.findAllOpenSixtySub();
                if (subscribers.size() == 0) {
                    return;
                }
                //只有区块的高度变化了，才触发获取logs的事件和触发取值事件
                Map<String, List<Subscriber>> subMap = subscribers.stream().collect(Collectors.groupingBy(Subscriber::getNetwork));
                for (String str : subMap.keySet()) {
//                    NetWorkEnum netWorkEnum = NetWorkEnum.getByName(str);
//                    if(netWorkEnum == null) continue;
//                    BlockHeight blockHeight = blockHeightService.getBySubId(netWorkEnum.getId());
//                    if(blockHeight == null){
//                        blockHeight = new BlockHeight();
//                        blockHeight.setModifyTime(LocalDateTime.now());
//                        blockHeight.setSubId(Integer.valueOf(netWorkEnum.getId()));
//                    }
//                    String filterId;
//                    if (StringUtils.isBlank(blockHeight.getFilterId()) || blockHeight.getModifyTime() == null || blockHeight.getModifyTime().isBefore(LocalDateTime.now().minus(15, ChronoUnit.MINUTES))) {
//                        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethNewBlockFilter(str);
//                        filterId = infuraResponseDto.getResult();
//                        blockHeight.setFilterId(filterId);
//                        blockHeight.setModifyTime(LocalDateTime.now());
//                        blockHeightService.saveOrUpdate(blockHeight);
//                    }
//                    filterId = blockHeight.getFilterId();


//                    InfuraResponseTransactionListDto infuraResponseTransactionListDto = InfuraMetodUtils.ethGetFilterChanges(str, filterId);
//
//                    if (infuraResponseTransactionListDto == null || infuraResponseTransactionListDto.getResult() == null) {
//                        log.info("sixty infuraResponseTransactionListDto is null");
//                        continue;
//                    }
//                    if (infuraResponseTransactionListDto.getResult().size() > 0) {

//                        blockHeight.setModifyTime(LocalDateTime.now());
//                        blockHeightService.saveOrUpdate(blockHeight);

                    Map<String, String> allMap = new HashMap<>();

                    List<Subscriber> subscriberList = subMap.get(str);
                    List<Subscriber> subscriberListForTran = subscriberList.stream().filter(v -> NoticeTypeEnum.TRAN.getType().equals(v.getNoticeType()) || NoticeTypeEnum.BATCH_TRAN.getType().equals(v.getNoticeType())).collect(Collectors.toList());
                    List<Subscriber> subscriberListForNum = subscriberList.stream().filter(v -> NoticeTypeEnum.NUM.getType().equals(v.getNoticeType())).collect(Collectors.toList());
                    log.info("sixty subscriberListForTran size:{}", subscriberListForTran.size());
                    log.info("sixty subscriberListForNum size:{}", subscriberListForNum.size());

                    int count = 0;
                    if (subscriberListForTran.size() > 0) {
                        count++;
                    }
                    if (subscriberListForNum.size() > 0) {
                        count++;
                    }
                    CountDownLatch countDownLatch = new CountDownLatch(count);
//                        log.info("[EventListener] countDownLatch countDownLatch size:{}", count);
                    if (subscriberListForTran.size() > 0) {
                        ThreadPoolUtils.filterExecutor.execute(() -> subscriberForTran(subscriberListForTran, allMap, countDownLatch));
                    }
                    if (subscriberListForNum.size() > 0) {
                        ThreadPoolUtils.filterExecutor.execute(() -> subscriberForNum(subscriberListForNum, allMap, countDownLatch));

                    }

                    countDownLatch.await();
//                        log.info("[EventListener] countDownLatch exec");
                    if (allMap.keySet().size() > 0) {
                        String lineSeparator = System.getProperty("line.separator");
                        StringJoiner stringJoiner = new StringJoiner("|" + lineSeparator);
                        for (String key : allMap.keySet()) {
                            stringJoiner.add(key + ":" + allMap.get(key));
                        }
                        log.info("[EventListener] sixty blockHeigh:{} values:{}", allMap.get("blockNo"), stringJoiner);
                    }

//                    }
                }

            } catch (ResourceAccessException e) {
                log.warn("EventListener Exception error e:", e);
            } catch (Exception e) {
                log.error("EventListener sixty Exception error e:", e);
            }
            log.info("[EventListener-2]end..");

        }, 3, 60, TimeUnit.SECONDS);


    }

    //交易类型订阅通知
    private void subscriberForTran(List<Subscriber> subscriberList, Map<String, String> allMap, CountDownLatch countDownLatch) {
        try {
            Map<String, String> netMap = new HashMap<>();
            subscriberList = subscriberList.stream().sorted(Comparator.comparing(Subscriber::getId)).collect(Collectors.toList());
            for (Subscriber subscriber : subscriberList) {
                BlockHeight blockHeight = null;
                try {
                    String fromBlock;
                    String toBlock;
                    String blockNo = netMap.get(subscriber.getNetwork());
                    log.info("network:{} blockNo:{} subId:{}", subscriber.getNetwork(), blockNo, subscriber.getId());
                    if (StringUtils.isBlank(blockNo)) {
                        blockNo = InfuraMetodUtils.ethBlockNumber(subscriber.getNetwork());
                        log.info("InfuraMetodUtils return network:{} blockNo:{}", subscriber.getNetwork(), blockNo);
                        if (StringUtils.isBlank(blockNo)) {
                            log.error("EventListener blockNo is null subId:{}", subscriber.getId());
                            continue;
                        }
                        netMap.put(subscriber.getNetwork(), blockNo);

                        allMap.put("blockNo", blockNo);
                    }


                    blockHeight = blockHeightService.getBySubId(subscriber.getId().toString());
                    if (blockHeight == null) {
                        blockHeight = new BlockHeight();
                        fromBlock = StringUtils.isBlank(subscriber.getFromBlock()) ? blockNo : subscriber.getFromBlock();
                        toBlock = blockNo;
                        blockHeight.setSubId(subscriber.getId());
                        blockHeight.setFromBlock(fromBlock);
                    } else {
                        if (StringUtils.isBlank(blockHeight.getToBlock())) {
                            fromBlock = blockHeight.getFromBlock();
                            blockHeight.setOriginBlock(null);
                        } else {
                            if (blockHeight.getToBlock().equals(blockNo)) {
                                continue;
                            }
                            blockHeight.setOriginBlock(blockHeight.getToBlock());
                            fromBlock = blockHeight.getToBlock();
                        }
                        toBlock = blockNo;
                    }

                    blockHeight.setToBlock(toBlock);

                    String filterId = blockHeight.getFilterId();
                    InfuraResponseTransactionListDto infuraResponseTransactionDto;
                    if (StringUtils.isBlank(blockHeight.getFilterId()) || blockHeight.getModifyTime() == null || blockHeight.getModifyTime().isBefore(LocalDateTime.now().minus(15, ChronoUnit.MINUTES))) {
                        InfuraParamsForFilterDto infuraParamsForFilterDto = new InfuraParamsForFilterDto();
                        infuraParamsForFilterDto.setFromBlock(fromBlock);
                        infuraParamsForFilterDto.setAddress(CommonUtil.addHexPrefixIfNotExist(subscriber.getAddress()));
                        if (subscriber.getTopics() != null && !subscriber.getTopics().equals("null") && StringUtils.isNotBlank(subscriber.getTopics())) {
                            try {
                                infuraParamsForFilterDto.setTopics(JacksonUtil.json2StringList(subscriber.getTopics()));
                            } catch (Exception e) {
                                log.error("EventListener topics is error subId:{}", subscriber.getId());
                            }
                        }
                        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethNewFilter(subscriber.getNetwork(), infuraParamsForFilterDto);
                        if (infuraResponseDto == null || StringUtils.isBlank(infuraResponseDto.getResult())) {
                            log.error("EventListener infuraResponseDto result is null subId:{}", subscriber.getId());
                            continue;
                        }
                        filterId = infuraResponseDto.getResult();
                        blockHeight.setFilterId(filterId);
                        blockHeight.setModifyTime(LocalDateTime.now());

                        infuraResponseTransactionDto = InfuraMetodUtils.ethGetFilterLogs(subscriber.getNetwork(), filterId);
                        if (infuraResponseTransactionDto == null || infuraResponseTransactionDto.getResult() == null || infuraResponseTransactionDto.getResult().size() == 0) {
//                            log.info("EventListener infuraResponseTransactionDto result is null subId:{}", subscriber.getId());
                            blockHeightService.saveOrUpdate(blockHeight);
                            continue;
                        }
                        log.info("infuraResponseTransactionDto.getResult:{}", JacksonUtil.obj2json(infuraResponseTransactionDto.getResult()));
                    } else {
                        infuraResponseTransactionDto = InfuraMetodUtils.ethGetFilterChanges(subscriber.getNetwork(), filterId);
                        if (infuraResponseTransactionDto.getResult() == null || infuraResponseTransactionDto.getResult().size() == 0) {
//                            log.info("EventListener infuraResponseTransactionDto result is null subId:{}", subscriber.getId());
                            blockHeight.setModifyTime(LocalDateTime.now());
                            blockHeightService.saveOrUpdate(blockHeight);
                            continue;
                        }
                    }


                    List<Transaction> transactionList = new ArrayList<>();
                    for (Object object : infuraResponseTransactionDto.getResult()) {
                        if (object == null) {
                            log.info("EventListener infuraResponseTransactionDto.getResult object is null subId:{}", subscriber.getId());
                        }
                        log.info("EventListener infuraResponseTransactionDto.getResult  is object:{} subId:{}", JacksonUtil.obj2json(object), subscriber.getId());
                        InfuraTransactionDto infuraTransactionDto = JacksonUtil.json2pojo(JacksonUtil.obj2json(object), InfuraTransactionDto.class);
                        if (infuraTransactionDto == null || StringUtils.isBlank(infuraTransactionDto.getTransactionHash())) {
                            log.info("EventListener infuraResponseTransactionDto.getResult is null subId:{}", subscriber.getId());
                            blockHeightService.saveOrUpdate(blockHeight);
                            continue;
                        }
                        //应用下已经通知成功的不再通知
                        Transaction transaction = transactionService.selectNoticeTransactionByHash(subscriber.getAppName(), infuraTransactionDto.getTransactionHash(), subscriber.getId());
                        if (transaction != null) {
                            log.info("EventListener transaction is success appName:{} hash:{}", subscriber.getAppName(), transaction.getTransactionHash());
                            blockHeightService.saveOrUpdate(blockHeight);
                            continue;
                        }
                        transaction = new Transaction();
                        BeanUtils.copyProperties(infuraTransactionDto, transaction);
                        transaction.setTopics(JacksonUtil.obj2json(infuraTransactionDto.getTopics()));
                        transaction.setBlockIntNum(Integer.parseInt(CommonUtil.removeHexPrefixIfExists(infuraTransactionDto.getBlockNumber()), 16));
                        transaction.setSubId(subscriber.getId());
                        transaction.setAppName(subscriber.getAppName());
                        transactionList.add(transaction);
                    }

                    if (transactionList.size() == 0) {
                        log.info("EventListener addTransactionAndBlockHeight listSize is zero subId:{}", subscriber.getId());
                        continue;
                    }

                    int i = transactionService.addTransactionAndBlockHeight(transactionList, blockHeight);
                    if (i != transactionList.size() + 1) {
                        log.error("EventListener addTransactionAndBlockHeight error i:{} transactionList:{} subId:{}", i, JacksonUtil.obj2json(transactionList), subscriber.getId());
                        continue;
                    }
                    //执行一次通知
                    ThreadPoolUtils.callbackExecutor.execute(() -> SyncTransactionSchedule.callBack(subscriber, transactionList));
                } catch (HttpServerErrorException e) {
                    log.warn("[subscriberForTran-Exception] e:", e);
                    if (blockHeight != null && blockHeight.getId() != null) {
                        log.warn("[subscriberForTran-Exception]blockHeight:{} e:", JacksonUtil.obj2json(blockHeight), e);
                        blockHeightService.updateFilterIdAndToBlockToNull(blockHeight.getId());
                    }
                } catch (Exception e) {
                    log.error("[subscriberForTran-Exception] e:", e);
                    if (blockHeight != null && blockHeight.getId() != null) {
                        log.error("[subscriberForTran-Exception]blockHeight:{} e:", JacksonUtil.obj2json(blockHeight), e);
                        blockHeightService.updateFilterIdAndToBlockToNull(blockHeight.getId());
                    }
                }
            }
            netMap.clear();
        } catch (Exception e) {
            log.error("[subscriberForTran]执行异常e:", e);
        } finally {
            log.info("[subscriberForTran]countDownLatch countDown");
            countDownLatch.countDown();
        }
    }

    //数值类型订阅通知
    private void subscriberForNum(List<Subscriber> subscriberList, Map<String, String> allMap, CountDownLatch countDownLatch) {

        try {
            String blockNo = InfuraMetodUtils.ethBlockNumber(subscriberList.get(0).getNetwork());
            for (Subscriber subscriber : subscriberList) {
                List<String> topics = JacksonUtil.json2StringList(subscriber.getTopics());
                boolean noticeAll = false;
                for (String topic : topics) {
                    CallParams callParams = CallParams.builder().to(subscriber.getAddress()).data(CommonUtil.addHexPrefixIfNotExist(topic)).build();
                    log.info("[subscriberForNum-ethCall-result] netWork:{}, blockNumber:{}, callParams:{}", subscriber.getNetwork(), "latest", JacksonUtil.obj2json(callParams));
                    InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethCall(subscriber.getNetwork(), callParams, "latest");
                    if (StringUtils.isBlank(infuraResponseDto.getResult())) {
                        log.info("[subscriberForNum-ethCall-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
                        continue;
                    }
                    SubNumValue subNumValue = subNumValueService.selectByFilterId(subscriber.getId());
                    if (subNumValue == null) {
                        subNumValue = new SubNumValue();
                        subNumValue.setNetWork(subscriber.getNetwork());
                        subNumValue.setAddress(subscriber.getAddress());
                        subNumValue.setTopic(topic);
                        subNumValue.setFilterId(subscriber.getId());
                    }
                    allMap.put(topic, infuraResponseDto.getResult());
                    if (!infuraResponseDto.getResult().equals(subNumValue.getValue())) {
                        BlockHeight blockHeight = blockHeightService.getBySubId(subscriber.getId().toString());
                        if (blockHeight == null) {
                            blockHeight = new BlockHeight();
                            String fromBlock = StringUtils.isBlank(subscriber.getFromBlock()) ? blockNo : subscriber.getFromBlock();
                            blockHeight.setSubId(subscriber.getId());
                            blockHeight.setFromBlock(fromBlock);
                        }

                        blockHeight.setToBlock(blockNo);
                        blockHeightService.saveOrUpdate(blockHeight);

                        subNumValue.setValue(infuraResponseDto.getResult());
                        subNumValueService.saveOrUpdate(subNumValue);

                        try {
                            // 调用回调接口 判断noticeUrl是否可用
                            HttpHeaders headers = new HttpHeaders();
                            headers.add("Content-Type", "application/json");
                            NoticeSubValueDto noticeSubValueDto = new NoticeSubValueDto();
                            noticeSubValueDto.setType(CommonUtil.removeHexPrefixIfExists(topic));
                            noticeSubValueDto.setValue(infuraResponseDto.getResult());
                            noticeSubValueDto.setSubId(subscriber.getId() + "");
                            noticeSubValueDto.setBlockNo(blockNo);
                            HttpEntity<String> r = new HttpEntity<>(JacksonUtil.obj2json(noticeSubValueDto), headers);
                            log.info("[subscriberForNum-event-subscribe]subId:{}noticeSubValueDto:{}", subscriber.getId(), JacksonUtil.obj2json(noticeSubValueDto));
                            RestTemplate restTemplate = SpringBeanUtil.getBean(RestTemplate.class);
                            if (restTemplate == null) {
                                restTemplate = new RestTemplate();
                            }
                            ResponseEntity<String> responseEntity = restTemplate.postForEntity(subscriber.getNoticeUrl(), r, String.class);
                            log.info("[subscriberForNum-event-subscribe]subId:{},response:{}", subscriber.getId(), responseEntity.getBody());
                            if (!"SUCCESS".equalsIgnoreCase(responseEntity.getBody())) {
                                log.info("[subscriberForNum]通知失败url:{}subId:{}return:{}", subscriber.getNoticeUrl(), subscriber.getId(), responseEntity.getBody());
                                subscriber.setNoticeErrTimes(subscriber.getNoticeErrTimes() + 1);
                                if (subscriber.getNoticeErrTimes() > 20) {
                                    log.error("[subscriberForNum-event-subscribe]subId:{},response:{}", subscriber.getId(), responseEntity.getBody());
                                    subscriber.setSubStatus(SubStatusEnum.CLOSE.getStatus());
                                    subscriber.setUpdateTime(LocalDateTime.now());
                                }
                                subscriberService.updateById(subscriber);
                            }
                            if ("SUCCESS".equals(responseEntity.getBody())) {
                                noticeAll = true;
                            }
                        } catch (Exception e) {
                            log.info("[subscriberForNum-event-subscribe]subId:{},request error", subscriber.getId());
                            subscriber.setNoticeErrTimes(subscriber.getNoticeErrTimes() + 1);
                            if (subscriber.getNoticeErrTimes() > 20) {
                                log.error("[subscriberForNum-event-subscribe]subId:{},CLOSE", subscriber.getId());
                                subscriber.setSubStatus(SubStatusEnum.CLOSE.getStatus());
                                subscriber.setUpdateTime(LocalDateTime.now());
                            }
                            subscriberService.updateById(subscriber);
                        }
                    }
                }
                //通知成功，查询当前subId有没有通知失败的，重新通知一次
//                if (noticeAll) {
//                    List<Transaction> transactions = SpringBeanUtil.getTransactionService().selectNoticeFailedTransactionBySubId(subscriber.getId());
//                    if (transactions != null && transactions.size() > 0) {
//                        transactions.forEach(v -> v.setNoticeTimes(0));
//                        SpringBeanUtil.getTransactionService().updateBatchById(transactions);
//                    }
//                }
            }
        } catch (Exception e) {
            log.error("[subscriberForNum]执行异常e:", e);
        } finally {
            log.info("[subscriberForNum]countDownLatch countDown");
            countDownLatch.countDown();
        }
    }

}


