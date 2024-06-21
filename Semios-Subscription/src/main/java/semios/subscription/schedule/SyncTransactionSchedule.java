package semios.subscription.schedule;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.client.RestTemplate;
import semios.subscription.listener.ThreadPoolUtils;
import semios.subscription.model.dto.response.InfuraResponseObjectDto;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.model.entity.Subscriber;
import semios.subscription.model.entity.Transaction;
import semios.subscription.model.enums.NoticeStatusEnum;
import semios.subscription.model.enums.NoticeTypeEnum;
import semios.subscription.model.enums.SubStatusEnum;
import semios.subscription.service.ISubscriberService;
import semios.subscription.service.ITransactionService;
import semios.subscription.utils.CommonUtil;
import semios.subscription.utils.InfuraMetodUtils;
import semios.subscription.utils.JacksonUtil;
import semios.subscription.utils.SpringBeanUtil;

import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @description: schedule
 * @author: xiangbin
 * @create: 2022-04-14 10:24
 **/
@Slf4j
@Configuration
@EnableScheduling
@EnableAsync
public class SyncTransactionSchedule {

    @Autowired
    private ISubscriberService subscriberService;

    @Autowired
    private ITransactionService transactionService;

    public static void callBack(Subscriber subscriber, List<Transaction> transactionList) {
        if (subscriber == null) {
            log.info("callBack subscriber is null!");
            return;
        }
        if (transactionList == null || transactionList.size() == 0) {
            transactionList = SpringBeanUtil.getTransactionService().selectNoticeTransactionBySubId(subscriber.getId());
        }
        if (transactionList.size() == 0) {
            log.info("callBack subscriber no transaction subId:{}", subscriber.getId());
            return;
        }
        //根据区块高度分组发送
        Map<String, List<Transaction>> blockMap = transactionList.stream().collect(Collectors.groupingBy(Transaction::getBlockNumber));
        for (String block : blockMap.keySet()) {
            //查询区块发生的时间
            String timestamp = null;
            InfuraResponseObjectDto infuraResponseDto = InfuraMetodUtils.ethGetBlockByNumber(subscriber.getNetwork(), block);
            if (infuraResponseDto.getResult() != null) {
                Map<String, Object> map = JacksonUtil.json2map(JacksonUtil.obj2json(infuraResponseDto.getResult()));
                if (map == null) {
                    log.error("callBack ethGetBlockByNumber map is null subId:{} block:{}", subscriber.getId(), block);
                    continue;
                }
                timestamp = (String) map.get("timestamp");
            }
            if (StringUtils.isBlank(timestamp)) {
                log.error("callBack subscriber no timestamp block:{}", block);
                continue;
            }
            timestamp = Integer.parseInt(CommonUtil.removeHexPrefixIfExists(timestamp), 16) + "";
            transactionList = blockMap.get(block);
            transactionList = transactionList.stream().sorted(Comparator.comparing(Transaction::getBlockIntNum).thenComparing(Transaction::getSubId)).collect(Collectors.toList());

            if (subscriber.getNoticeType().equals(NoticeTypeEnum.BATCH_TRAN.getType())) {
                Map<String, List<Transaction>> transactionMap = transactionList.stream().collect(Collectors.groupingBy(Transaction::getTransactionHash));
                for (String s : transactionMap.keySet()) {
                    List<Transaction> transactions = transactionMap.get(s);
                    try {
                        // 调用接口
                        HttpHeaders headers = new HttpHeaders();
                        headers.add("Content-Type", "application/json");
                        log.info("[callBack subscriber]noticeUrl:{} transaction size:{}", subscriber.getNoticeUrl(), transactions.size());
                        HttpEntity<String> r = new HttpEntity<>("{\"result\":" + JacksonUtil.obj2json(transactions) + ",\"timestamp\":" + timestamp + "}", headers);
                        RestTemplate restTemplate = SpringBeanUtil.getBean(RestTemplate.class);
                        if (restTemplate == null) {
                            log.warn("[callBack subscriber]restTemplate is null noticeUrl:{}", subscriber.getNoticeUrl());
                            restTemplate = new RestTemplate();
                        }
                        ResponseEntity<String> responseEntity = restTemplate.postForEntity(subscriber.getNoticeUrl(), r, String.class);
                        log.info("[callBack subscriber]noticeUrl:{},subId:{} response:{} ", subscriber.getNoticeUrl(), transactions.get(0).getSubId(), responseEntity.getBody());

                        if ("SUCCESS".equals(responseEntity.getBody())) {
                            String timestamp1 = timestamp;
                            transactions.forEach(v -> {
                                v.setNoticeStatus(NoticeStatusEnum.SUCCESS.getStatus());
                                v.setBlockTimestamp(timestamp1);
                            });
                        } else {
                            String timestamp1 = timestamp;
                            transactions.forEach(v -> {
                                v.setBlockTimestamp(timestamp1);
                                v.setNoticeTimes(v.getNoticeTimes() == null ? 1 : v.getNoticeTimes() + 1);
                            });
                        }
                        SpringBeanUtil.getTransactionService().updateBatchById(transactions);

                    } catch (Exception e) {
                        log.error("[callBack subscriber]transactionHash:{}e:", transactions.get(0).getTransactionHash(), e);
                    }
                }

            } else {
                boolean noticeAll = false;
                for (Transaction transaction : transactionList) {
                    try {
                        // 调用接口
                        HttpHeaders headers = new HttpHeaders();
                        headers.add("Content-Type", "application/json");
                        HttpEntity<String> r = new HttpEntity<>("{\"result\":[" + JacksonUtil.obj2json(transaction) + "],\"timestamp\":" + timestamp + "}", headers);
//                    log.info("[callBack subscriber]noticeUrl:{} transaction:{}", subscriber.getNoticeUrl(), JacksonUtil.obj2json(transaction));
                        RestTemplate restTemplate = SpringBeanUtil.getBean(RestTemplate.class);
                        if (restTemplate == null) {
                            log.warn("[callBack subscriber]restTemplate is null noticeUrl:{}", subscriber.getNoticeUrl());
                            restTemplate = new RestTemplate();
                        }
                        ResponseEntity<String> responseEntity = restTemplate.postForEntity(subscriber.getNoticeUrl(), r, String.class);
                        log.info("[callBack subscriber]noticeUrl:{},response:{} subId:{}", subscriber.getNoticeUrl(), responseEntity.getBody(), transaction.getSubId());
                        if (!"SUCCESS".equals(responseEntity.getBody())) {
                            transaction.setNoticeStatus(NoticeStatusEnum.FAIL.getStatus());
                            if (transaction.getNoticeTimes() == null) {
                                transaction.setNoticeTimes(1);
                            } else {
                                transaction.setNoticeTimes(transaction.getNoticeTimes() + 1);
                            }
                            if (transaction.getNoticeTimes() == 3) {
                                log.info("callBack subscriber notice time over 3,please be careful！transactionId:{}", transaction.getId());
                                subscriber.setNoticeErrTimes(subscriber.getNoticeErrTimes() + 1);
                                if (subscriber.getNoticeErrTimes() >= 20) {
                                    log.error("callBack subscriber notice time over 10,please be careful！subId:{}", subscriber.getId());
                                    subscriber.setSubStatus(SubStatusEnum.CLOSE.getStatus());
                                }
                                SpringBeanUtil.getSubscriberService().updateById(subscriber);
                            }
                        } else {
                            transaction.setNoticeStatus(NoticeStatusEnum.SUCCESS.getStatus());
                        }
                        transaction.setBlockTimestamp(timestamp);
                        SpringBeanUtil.getTransactionService().updateById(transaction);

                        //通知成功，查询当前subId有没有通知失败的，重新通知一次
                        if ("SUCCESS".equals(responseEntity.getBody())) {
                            noticeAll = true;
                        }

                    } catch (Exception e) {
                        log.error("[callBack subscriber]transactionHash:{}e:", transaction.getTransactionHash(), e);
                        transaction.setNoticeStatus(NoticeStatusEnum.FAIL.getStatus());
                        if (transaction.getNoticeTimes() == null) {
                            transaction.setNoticeTimes(1);
                        } else {
                            transaction.setNoticeTimes(transaction.getNoticeTimes() + 1);
                        }
                        if (transaction.getNoticeTimes() == 3) {
                            log.info("callBack subscriber notice time over 3,please be careful！transactionId:{}", transaction.getId());
                            subscriber.setNoticeErrTimes(subscriber.getNoticeErrTimes() + 1);
                            if (subscriber.getNoticeErrTimes() >= 20) {
                                log.error("callBack subscriber notice time over 20,please be careful！subId:{}", subscriber.getId());
                                subscriber.setSubStatus(SubStatusEnum.CLOSE.getStatus());
                            }
                            SpringBeanUtil.getSubscriberService().updateById(subscriber);
                        }
                        transaction.setBlockTimestamp(timestamp);
                        SpringBeanUtil.getTransactionService().updateById(transaction);
                    }
                }
                /*if (noticeAll) {
                    if (subscriber.getNoticeErrTimes() >= 10) {
                        subscriber.setNoticeErrTimes(0);
                        SpringBeanUtil.getSubscriberService().updateById(subscriber);
                    }
                    List<Transaction> transactions = SpringBeanUtil.getTransactionService().selectNoticeFailedTransactionBySubId(subscriber.getId());
                    if (transactions != null && transactions.size() > 0) {
                        transactions.forEach(v -> v.setNoticeTimes(0));
                        SpringBeanUtil.getTransactionService().updateBatchById(transactions);
                    }
                }*/
            }

        }
    }

    public static void main(String[] args) {
//        System.out.println("0x5d12ab".substring(2));
//        System.out.println(Integer.parseInt("0x5d12ab".substring(2), 16));
//        String str = "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"baseFeePerGas\":\"0x118c51ed74\",\"difficulty\":\"0x1303d607a\",\"extraData\":\"0xd883010a10846765746888676f312e31372e35856c696e7578\",\"gasLimit\":\"0x7a1200\",\"gasUsed\":\"0x4e0a60\",\"hash\":\"0xdde2160fdce45105cb8e119b3234befc6004dff78038e806526ed6697671bd12\",\"logsBloom\":\"0x108000000000c0000000200000000022000008000005260040000000022001000801040000000100400000000000000000080001810080a0000000098020000c0000000080002800000000080000800000008800000000000000000008000000420004400200040000400400004008000000b00060a88000c000201000000000000800000024000000080402200000000080000400490010204000004081000a020004200000000000800000010000000000100000000000000000000000040000010082100000801000002000020000000008008080000000000300000020000010200802002000001060002040080000001000000080000100000004014140\",\"miner\":\"0x637ee9752eb0dace27a9b0b7556ffaf96b847480\",\"mixHash\":\"0x2337c3661696d5df71dec07bf3d59d03bdda5a6eb5093a6d30af61e67ca7f4e5\",\"nonce\":\"0x80ea3d24a1161851\",\"number\":\"0xba406a\",\"parentHash\":\"0xe35fab9f5308e1eb9a78f3512ec536dadd2b062f702c9c5ee3c8d6d1f7c390c7\",\"receiptsRoot\":\"0x096a0b118151d1aae57321f0ea740e9706705f015fceb7817abaf315210a8aed\",\"sha3Uncles\":\"0x79a275d1e2ef66bcf1b9c7968c7aa6a8102df04a9c42b55c0d92b6de88a21921\",\"size\":\"0x8078\",\"stateRoot\":\"0x5f7f0906f0fa79daec16bc10e02308c065ee74bacb261219e319f79ca7bb8482\",\"timestamp\":\"0x625ef4ee\",\"totalDifficulty\":\"0x92aab1b493107b\",\"transactions\":[\"0x8b8181491550e80e3175d61aa595071a3632271a55ea4e9535d34918c004dd81\",\"0x57f081adf7d488cc5484d1222b7844f9042d994a90526a35f1b54364732e9910\",\"0x3603e589cbfcf62c563654f26f6497cdb646b7351c240b15050e83610b0e1b84\",\"0x7398584d8c3abc2d5af3f79a6b43b986f5e1145206215958d2cece7aabc13c0c\",\"0x1008072a3ae5420673c88b7d15baac8c8f6eeb9d0c3e2fd6cae83dc93416db4f\",\"0x99bcb2e87ffe72f6ba88677941bc62bad91ad4ae81b1f97a775967cefb6ee74b\",\"0xd9fa90bed970832103deb637d10ed082ea09b241b9dee6bf428a35cb4fc7bd3f\",\"0xaede21f7f85cc04e2d315834e6986704d36289eece17bdf1e2a9f6d7e22746b6\",\"0x54cc5437a5553814f1fe1e07b418f85e92fe754f5f1a3e57ec6eee87d462a1d5\",\"0x853eb6b32735c822ad9e3a7b48e529ad465749472e81a6b4a91b988ba462a50e\",\"0xb9bdce5d05a5e87b75853c867cddde54e2a6beaf41d19bd8c452260ddeff1353\",\"0x67c6b8634dcc73ceb6cd258cb8d6b6b89be73b281c9fb2d1eba633486821dd78\",\"0x952f17841b1d0103e83586e12997f31fff67d4ebd3d61da4f785f4972acf4ddc\",\"0xad0754a36f13e979f63c124079468e8a7cbbc43819a274dbe6b6f7a73ff9cc4c\",\"0x06cbc41f6daf894241c40b5a2b0d2102535e473ba9c82ea12f86e4d8a2521501\",\"0x8eba035d412a844284ad97a0cc857fb03838c97176e7931bdf80a599f43c282e\",\"0xa06f6dfabf97da2fb4ef567343219b19dd2d17a0dab999ca33f1fb86b9b0a7f1\"],\"transactionsRoot\":\"0xf70da27f3a94864c8d92f564ee7aefd062ea66383f92d19384fbda2779923157\",\"uncles\":[\"0x8e1ca2b488752d82c7248151518f137a405a150ead300588a64813a74959d396\"]}}";
//        InfuraResponseObjectDto infuraResponseDto = JacksonUtil.json2pojo(str, InfuraResponseObjectDto.class);
//        Map map = JacksonUtil.json2map(JacksonUtil.obj2json(infuraResponseDto.getResult()));
//        System.out.println(map.get("timestamp"));
//        System.out.println(Integer.parseInt("0000000000000000000000000000000000000000000746fcbbe7d269c8a726e1", 16));
//        BigInteger bigInteger = new BigInteger("ghk", 16);
//        System.out.println(bigInteger);
        BlockHeight blockHeight = new BlockHeight();
        blockHeight.setModifyTime(LocalDateTime.now());
        System.out.println(JacksonUtil.obj2json(blockHeight));

    }

    @Async
    @Scheduled(cron = "0/20 * * * * ? ")
    public void syncTransaction() {//2分钟执行一次

        List<Transaction> transactionList = transactionService.selectNoticeTransaction();
        if (transactionList.size() == 0) {
            log.info("[syncTransaction] no transaction");
            return;
        }

        Map<Integer, List<Transaction>> stringListMap = transactionList.stream().collect(Collectors.groupingBy(Transaction::getSubId));
        for (Integer subId : stringListMap.keySet()) {
            Subscriber subscriber = subscriberService.getById(subId);
            if (subscriber == null || subscriber.getSubStatus().equals(SubStatusEnum.CLOSE.getStatus())) {
                List<Transaction> transactions = stringListMap.get(subId);
                if (transactions != null && transactions.size() > 0) {
                    log.info("[SyncTransactionSchedule-syncTransaction]over time size:{}", transactions.size());
                    transactions.forEach(v -> v.setNoticeTimes(4));
                    transactionService.updateBatchById(transactions);
                }
                continue;
            }
            ThreadPoolUtils.callbackExecutor.execute(() -> SyncTransactionSchedule.callBack(subscriber, stringListMap.get(subId)));
        }
    }
}
