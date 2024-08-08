package semios.api.schedule;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.http.*;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.client.RestTemplate;
import org.web3j.crypto.Hash;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.request.SubscribeRequestDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.model.vo.req.Erc20LiquidityReqVo;
import semios.api.model.vo.req.LiquidityTransactionReqVo;
import semios.api.model.vo.req.UserLiquidityStatisticsVo;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.IProtoDaoDexService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.*;

import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description: gas
 * @author: xiangbin
 * @create: 2022-04-24 13:48
 **/
@Slf4j
@Configuration
@EnableScheduling
@EnableAsync
public class ProtoDaoSchedule {

    private static final RestTemplate REST_TEMPLATE = new RestTemplate();
    private static int number = 0;
    @Autowired
    private ISubscribeService subscribeService;
    @Autowired(required = false)
    private ISubscriptionService subscriptionService;
    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;
    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;
    @Autowired
    private CommonService commonService;
    @Autowired
    private IWorkService workService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;
    @Autowired
    private IUserService userService;
    @Autowired(required = false)
    private IProtoDaoDexService protodaoDexService;
    @Autowired
    private TaskExecutor taskExecutor;
    @Autowired
    private S3Service s3Service;

    @Autowired
    private IDaoDailyStatisticsService daoDailyStatisticsService;

    public static void main(String[] args) throws Exception {
//        String resBody = "{\n" +
//                "    \"execution_id\": \"01H3VG9AH58J7WERDXCJJEPVP6\",\n" +
//                "    \"query_id\": 2665201,\n" +
//                "    \"state\": \"QUERY_STATE_COMPLETED\",\n" +
//                "    \"submitted_at\": \"2023-06-26T09:24:18.085594Z\",\n" +
//                "    \"expires_at\": \"2023-09-24T09:24:26.031086Z\",\n" +
//                "    \"execution_started_at\": \"2023-06-26T09:24:18.173293Z\",\n" +
//                "    \"execution_ended_at\": \"2023-06-26T09:24:26.031085Z\",\n" +
//                "    \"result\": {\n" +
//                "        \"rows\": [\n" +
//                "            {\n" +
//                "                \"eth\": 639.142156\n" +
//                "            }\n" +
//                "        ],\n" +
//                "        \"metadata\": {\n" +
//                "            \"column_names\": [\n" +
//                "                \"eth\"\n" +
//                "            ],\n" +
//                "            \"result_set_bytes\": 11,\n" +
//                "            \"total_row_count\": 1,\n" +
//                "            \"datapoint_count\": 1,\n" +
//                "            \"pending_time_millis\": 87,\n" +
//                "            \"execution_time_millis\": 7857\n" +
//                "        }\n" +
//                "    }\n" +
//                "}";
//
//        Map<String, Object> resMap = JacksonUtil.json2map(resBody);
//        Object result = resMap.get("result");
//        Map<String, Object> resultMap = JacksonUtil.json2map(JacksonUtil.obj2json(result));
//        Object rows = resultMap.get("rows");
//        List<Map> rowsMapList = JacksonUtil.json2list(JacksonUtil.obj2json(rows), Map.class);
//        String eth = String.valueOf(rowsMapList.get(0).get("eth"));
//
//        System.out.println(eth);
//
//        BigDecimal ethBigDecimal = new BigDecimal(eth).setScale(4, RoundingMode.FLOOR);
//        ethBigDecimal = ethBigDecimal.multiply(new BigDecimal("1250").divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE), 4, RoundingMode.FLOOR));
//        System.out.println(ethBigDecimal);

        Timestamp today = DateUtil.getBeginOfToday();
        long todayBeginHour = DateUtil.getTimestampAfterDay(today, 0).getTime() / 1000;
        long yesterdayBeginHour = DateUtil.getTimestampAfterDay(today, -1).getTime() / 1000;
        System.out.println(todayBeginHour);
        System.out.println(yesterdayBeginHour);


    }

    @Async
    @Scheduled(cron = "0 0/5 * * * ? ")
    public void subscribeStatus() {// 每5分钟执行一次
        log.info("[subscribeStatus] running...");
        List<Subscribe> subscribeList = subscribeService.selectByStatusStoped();
        if (subscribeList == null || subscribeList.size() == 0) {
            return;
        }
        log.info("[subscribeStatus] subscribeList size:{}", subscribeList.size());

        for (Subscribe subscribe : subscribeList) {
            try {
                TradeTypeEnum tradeTypeEnum = TradeTypeEnum.queryByType(subscribe.getTradeType());
                if (tradeTypeEnum == null) {
                    log.error("[ProtoDaoSchedule]tradeTypeEnum is null subId:{}", subscribe.getId());
                    continue;
                }
                SubscriberTypeEnum subscriberTypeEnum = tradeTypeEnum.getSubscriberTypeEnum();
                if (subscriberTypeEnum == null) {
                    log.error("[ProtoDaoSchedule] subscriberTypeEnum is null subId:{}", subscribe.getId());
                    continue;
                }
                // 值类型的
                if (tradeTypeEnum.getLocal()
                        && subscriberTypeEnum.getType().equals(SubscriberTypeEnum.VALUE.getType())) {
                    // 本地存储的值
                    String value = ProtoDaoCommonUtil.ethCall(subscriptionService, ProtoDaoConstant.netWork,
                            subscribe.getContractAddress(), subscribe.getTopics());
                    log.info("ProtoDaoSchedule return subscribe:{}, value:{}", subscribe.getTradeType(), value);
                    if (StringUtils.isBlank(value)) {
                        log.error("[ProtoDaoSchedule]tradeTypeEnum value is null subId:{}", subscribe.getId());
                        continue;
                    }
                    TradeTypeEnum.setDefaultValue(tradeTypeEnum, value);
                    subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribeService.updateById(subscribe);
                    continue;
                }
                //
                String fromBlock = subscribe.getFromBlock();
                // 需要订阅的值
                if (StringUtils.isNotBlank(subscribe.getFilterId())) {
                    Result<String> result = subscriptionService.subscripeStatus(subscribe.getFilterId());
                    if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                        log.error("[ProtoDaoSchedule] subscripeStatus error filterId:{} status resultDesc:{}",
                                subscribe.getFilterId(), result.getResultDesc());
                        continue;
                    }
                    if (result.getData().equalsIgnoreCase("1")) {// 0-关闭 1-开启 开启则不用管，如果已经关闭了，则查询上次通知的区块，然后重新订阅
                        log.info("[ProtoDaoSchedule] subId:{} status is ok!", subscribe.getId());
                        subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                        subscribeService.updateById(subscribe);
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
                subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + ProtoDaoConstant.appName);
                Result<String> result = subscriptionService.subscripe(subscribeRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[ProtoDaoSchedule] subscripe retry error filterId:{} status resultDesc:{}",
                            subscribe.getFilterId(), result.getResultDesc());
                    subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
                } else {
                    log.info("[ProtoDaoSchedule] subscribeId:{} return subId:{}", subscribe.getId(), result.getData());
                    subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribe.setFilterId(result.getData());
                }
                subscribeService.updateById(subscribe);

            } catch (Exception e) {
                log.error("[ProtoDaoSchedule] subscripe retry error filterId:{} status e:{}", subscribe.getFilterId(),
                        e);
            }
        }
    }

//    @Async
//    @Scheduled(cron = "0 0/10 * * * ? ")
//    public void handleCurrentDrb() {// 每10分钟执行一次
//        if (StringUtils.isBlank(ProtoDaoConstant.CURRENT_ROUND)) {
//            log.error("[handleCurrentDrb] error CURRENT_ROUND is null:{} ", ProtoDaoConstant.CURRENT_ROUND);
//            return;
//        }
//        Integer drbNumber = Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND);
//
//        // 获取currentRound()方法
//        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
//        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
//        infuraCallRequestDto.setTo(ContractMethodEnum.CURRENT_ROUND.getContractAddress());
//        infuraCallRequestDto.setData(ContractMethodEnum.CURRENT_ROUND.getMethodAddress());
//
//        Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
//
//        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[handleCurrentDrb] error result:{} ", result.getResultDesc());
//            return;
//        }
//
//        String resultData = result.getData();
//        String drbNum = CommonUtil.hexToTenString(resultData);
//        if (StringUtils.isNotBlank(drbNum) && !Integer.valueOf(drbNum).equals(drbNumber)) {
//            log.info("[handleCurrentDrb]infura return data:{} drbNumber:{} number:{}", result.getData(), drbNumber, number);
//            number++;
//            if (number > 2) {
//                log.info("[handleCurrentDrb] executor change drb current_round:{} number:{}", drbNum, number);
//                ProtoDaoConstant.CURRENT_ROUND = drbNum;
//                // 做drb结算，变更drb
////                SearchController.searchExecutor.execute(() -> commonService.currentDrbDaoStatistics(drbNumber));
//                taskExecutor.execute(() -> commonService.currentDrbDaoStatistics(drbNumber));
//                commonService.handleNextDrbStartBlock(Integer.valueOf(drbNum));
//                // 做dao的开始和关闭操作
////                if (StringUtils.isNotBlank(drbNum)) {
////                    SearchController.searchExecutor.execute(() -> commonService.startDao(Integer.valueOf(drbNum)));
////                    SearchController.searchExecutor.execute(() -> commonService.createCurrentDrbDaoStatistics(Integer.valueOf(drbNum)));
////                }
//                number = 0;
//            }
//        } else if (StringUtils.isNotBlank(drbNum) && Integer.valueOf(drbNum).equals(drbNumber)) {
//            //查询上一个drb是否计算完DaoDrbStatistics
//            //执行条件 上一个drb有铸造work，但是没有计算完成的DaoDrbStatistics
//            DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
//            Page<Work> iPage = new Page<>(1, 10);
//            daoSortedReqVo.setCurrentDrb(drbNumber - 1);
//            Page<Work> workPage = workService.selectDrbNfts(iPage, daoSortedReqVo);
//            List<Work> works = workPage.getRecords();
//            if (!works.isEmpty()) {
//                List<DaoDrbStatistics> daoDrbStatisticsList = daoDrbStatisticsService.selectByDrbNumber(drbNumber - 1);
//                daoDrbStatisticsList = daoDrbStatisticsList.stream().filter(v -> Integer.valueOf(2).equals(v.getStatus())).collect(Collectors.toList());
//                if (daoDrbStatisticsList.isEmpty()) {
//                    log.info("[handleCurrentDrb] second infura return data:{} drbNumber:{} number:{}", result.getData(), drbNumber, number);
//                    number++;
//                    if (number > 2) {
//                        log.info("[handleCurrentDrb] second executor change drb current_round:{} number:{}", drbNumber, number);
//                        taskExecutor.execute(() -> commonService.currentDrbDaoStatistics(drbNumber - 1));
//                        commonService.handleNextDrbStartBlock(drbNumber);
//
//                        number = 0;
//                    }
//                }
//            }
//        }
//
//    }

    @Async
    @Scheduled(cron = "0 0/10 * * * ? ")
    public void handleFailStatistics() {// 每10分钟执行一次
        Integer drbNumber = Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) - 1;
        List<DaoDrbStatistics> daoDrbStatisticsList = daoDrbStatisticsService.selectFailStatus(drbNumber);
        if (!daoDrbStatisticsList.isEmpty()) {
            log.info("[handleFailStatistics]dao size:{}", daoDrbStatisticsList.size());
            daoDrbStatisticsList.forEach(v -> v.setTimes(v.getTimes() + 1));
            commonService.handleDaoDrbStatistics(daoDrbStatisticsList, drbNumber);
        }

        List<CanvasDrbStatistics> canvasDrbStatisticsList = canvasDrbStatisticsService.selectFailStatus(drbNumber);

        if (!canvasDrbStatisticsList.isEmpty()) {
            log.info("[handleFailStatistics]canvas size:{}", canvasDrbStatisticsList.size());
            canvasDrbStatisticsList.forEach(v -> v.setTimes(v.getTimes() + 1));
            commonService.handleCanvasDrbStatistics(canvasDrbStatisticsList, drbNumber);
            // 计算当前drb铸造收益
            commonService.handleCurrentDrbMinterProfit(canvasDrbStatisticsList, drbNumber);
        }

    }

    @Async
    @Scheduled(cron = "0 0 0/1 * * ? ")
    public void freshOpenseaApi() {// 每小时执行一次
        log.info("[freshOpenseaApi] running...");

        Long time = System.currentTimeMillis() / 1000 - 7200;
        log.info("[freshOpenseaApi] time:{}", time);
        List<Work> nfts = workService.selectNftNearlyTwoHours(time);
        log.info("[freshOpenseaApi] nfts size:{}", nfts.size());

        for (Work nft : nfts) {
            Dao dao = daoService.daoDetailByDaoId(nft.getDaoId());
            // 刷新oponsea名称
            String openseaUrl =
                    String.format(ProtoDaoConstant.openseaApiNftLink, dao.getErc721Token(), nft.getWorkNumber());
            log.info("[freshOpenseaApi]openseaUrl:{}", openseaUrl);
            HttpHeaders headers = new HttpHeaders();
            headers.set("user-agent", "Chrome/83.0.4103.116");
            if (StringUtils.isNotBlank(ProtoDaoConstant.openseaApiKey)) {
                headers.set("X-API-KEY", ProtoDaoConstant.openseaApiKey);
            }
            HttpEntity<String> httpEntity = new HttpEntity<>(headers);

            try {
                ResponseEntity<String> responseEntity =
                        REST_TEMPLATE.exchange(openseaUrl, HttpMethod.POST, httpEntity, String.class);

                String response = responseEntity.getBody();
                if (responseEntity.getStatusCode() != HttpStatus.OK || response == null || !response.contains(ProtoDaoConstant.openseaApiSuccess)) {
                    log.error("[D4AMintNFTChainService] fresh openseaUrl:{} code:{} body:{}", openseaUrl,
                            responseEntity.getStatusCode(), responseEntity.getBody());
                    continue;
                }
                // Map<String, Object> stringObjectMap = JacksonUtil.json2map(responseEntity.getBody());
                // String name = (String) stringObjectMap.get("name");
                log.info("[freshOpenseaApi]response:{}", response);

            } catch (Exception e) {
                log.error("[freshOpenseaApi] fresh openseaUrl:{} error:{}", openseaUrl, e);
            }

        }

    }

    // opensea的collection不支持刷新 暂停任务
    //@Async
    //@Scheduled(cron = "0 0/5 * * * ? ")
    public void freshDaoOpenseaApi() {// 每五分钟执行一次
        log.info("[freshDaoOpenseaApi] running...");

        List<Dao> daoList = daoService.freshDaoOpenseaApi();
        if (daoList.size() > 0) {
            log.info("[freshDaoOpenseaApi] size:{}", daoList.size());
            for (Dao dao : daoList) {
                // 刷新oponsea名称
                String openseaUrl = String.format(ProtoDaoConstant.openseaApiDaoLink, dao.getErc721Token());
                log.info("[freshDaoOpenseaApi]openseaUrl:{}", openseaUrl);
                HttpHeaders headers = new HttpHeaders();
                headers.set("user-agent", "Chrome/83.0.4103.116");
                if (StringUtils.isNotBlank(ProtoDaoConstant.openseaApiKey)) {
                    headers.set("X-API-KEY", ProtoDaoConstant.openseaApiKey);
                }
                HttpEntity<String> httpEntity = new HttpEntity<>(headers);

                ResponseEntity<String> responseEntity =
                        REST_TEMPLATE.exchange(openseaUrl, HttpMethod.GET, httpEntity, String.class);
                if (responseEntity.getStatusCode() != HttpStatus.OK) {
                    log.error("[freshDaoOpenseaApi] refresh opensea error param:{} error:{}", JacksonUtil.obj2json(dao),
                            responseEntity.getBody());
                } else {
                    Dao updateDao = new Dao();
                    updateDao.setId(dao.getId());
                    updateDao.setFreshOpensea(0);
                    daoService.updateById(updateDao);
                }
            }
        }

        log.info("[freshDaoOpenseaApi] ending...");

    }

    @Async
    @Scheduled(cron = "0 0/5 * * * ? ")
    public void freshUserIsContract() {// 每五分钟执行一次
        log.info("[freshUserIsContract] running...");

        List<User> userList = userService.findUsersIsContractNull();
        log.info("[freshUserIsContract] size:{}", userList.size());
        if (userList.size() > 0) {
            for (User user : userList) {
                Result<Boolean> resultBoolean =
                        subscriptionService.ethGetCodeCheckUserAddress(ProtoDaoConstant.netWork, user.getUserAddress());
                if (resultBoolean != null && resultBoolean.getData()) {
                    user.setIsContract(0);
                } else {
                    user.setIsContract(1);
                }
                userService.updateById(user);
            }
        }
        log.info("[freshUserIsContract] ending...");

    }

    @Async
    @Scheduled(cron = "0 0/5 * * * ? ")
    public void freshDaoSymbolAndErc20Name() {// 每五分钟执行一次
        log.info("[freshDaoSymbolAndErc20Name] running...");
        List<Dao> daoList = daoService.freshDaoSymbolAndErc20Name();
        if (daoList.size() > 0) {
            log.info("[freshDaoSymbolAndErc20Name] size:{}", daoList.size());
            for (Dao dao : daoList) {
                String erc20Token = dao.getErc20Token();
                Dao updateDao = new Dao();
                updateDao.setId(dao.getId());

                if (StringUtils.isNotBlank(erc20Token) && StringUtils.isBlank(dao.getDaoSymbol())) {
                    try {
                        InfuraCallRequestDto infuraCallRequestDto2 = new InfuraCallRequestDto();
                        infuraCallRequestDto2.setNetWork(ProtoDaoConstant.netWork);
                        infuraCallRequestDto2.setTo(erc20Token);
                        infuraCallRequestDto2.setData(ContractMethodEnum.DAO_SYMBOL.getMethodAddress());

                        // 调用查询使用数据集的user
                        Result<String> resul2 = subscriptionService.infuraCall(infuraCallRequestDto2);
                        if (resul2.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                            log.error("[freshDaoSymbolAndErc20Name] dao symbol error result:{}",
                                    resul2.getResultDesc());
                            throw new RuntimeException("保存project查询symbol信息失败");
                        }
                        log.info("[freshDaoSymbolAndErc20Name]infura symbol return data:{}", resul2.getData());
                        String projectInfoData2 = CommonUtil.removeHexPrefixIfExists(resul2.getData());
                        List<String> dataList2 = CommonUtil.splitBy32Bytes(projectInfoData2);
                        String daoSymbol = CommonUtil.dynamicArgumentDecoding(projectInfoData2, dataList2.get(0), true);
                        updateDao.setDaoSymbol(daoSymbol);
                    } catch (Exception e) {
                        log.error("[freshDaoSymbolAndErc20Name]dao symbol erc20_token:{} exception:{}", erc20Token, e);
                        continue;
                    }
                }
                if (StringUtils.isNotBlank(erc20Token) && StringUtils.isBlank(dao.getErc20Name())) {
                    try {
                        InfuraCallRequestDto infuraCallRequestDto3 = new InfuraCallRequestDto();
                        infuraCallRequestDto3.setNetWork(ProtoDaoConstant.netWork);
                        infuraCallRequestDto3.setTo(erc20Token);
                        infuraCallRequestDto3.setData(ContractMethodEnum.NAME.getMethodAddress());

                        // 查询erc20Name
                        Result<String> resul3 = subscriptionService.infuraCall(infuraCallRequestDto3);
                        if (resul3.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                            log.error("[freshDaoSymbolAndErc20Name] erc20 name error result:{}",
                                    resul3.getResultDesc());
                            throw new RuntimeException("保存project查询erc20name信息失败");
                        }
                        log.info("[freshDaoSymbolAndErc20Name]infura erc20 name return data:{}", resul3.getData());
                        String projectInfoData3 = CommonUtil.removeHexPrefixIfExists(resul3.getData());
                        List<String> dataList3 = CommonUtil.splitBy32Bytes(projectInfoData3);
                        String erc20Name = CommonUtil.dynamicArgumentDecoding(projectInfoData3, dataList3.get(0), true);
                        updateDao.setErc20Name(erc20Name);
                    } catch (Exception e) {
                        log.error("[freshDaoSymbolAndErc20Name]erc20 name erc20_token:{} exception:{}", erc20Token, e);
                        continue;
                    }
                }
                daoService.updateById(updateDao);

            }
        }

        log.info("[freshDaoSymbolAndErc20Name] ending...");
    }

    @Async
    @Scheduled(cron = "0/10 * * * * ? ")
    public void syncBurnTransaction() {
        log.info("[syncBurnTransaction] running...");
        List<TokenReceivedRecord> tokenReceivedRecordList = tokenReceivedRecordService.syncDexForBurn();
        if (tokenReceivedRecordList.size() > 0) {
            log.info("[syncBurnTransaction] size:{}", tokenReceivedRecordList.size());
            Map<String, List<TokenReceivedRecord>> transactionMap = tokenReceivedRecordList.stream()
                    .collect(Collectors.groupingBy(TokenReceivedRecord::getTransactionHash));
            for (String transactionHash : transactionMap.keySet()) {
                List<TokenReceivedRecord> tokenReceivedRecordList1 = transactionMap.get(transactionHash);
                BigDecimal inTokenAmount = tokenReceivedRecordList1.stream().map(TokenReceivedRecord::getTokenNum)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);
                BigDecimal outTokenAmount = tokenReceivedRecordList1.stream().map(TokenReceivedRecord::getEthAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);
                TokenReceivedRecord tokenReceivedRecord = tokenReceivedRecordList1.get(0);
                Dao dao = daoService.selectDaoByDaoNumber(tokenReceivedRecord.getDaoNumber());
                if (dao == null) {
                    log.error("[syncBurnTransaction] dao is null for tokenReceivedRecord:{}",
                            JacksonUtil.obj2json(tokenReceivedRecord));
                    continue;
                }
                LiquidityTransactionReqVo liquidityTransaction = new LiquidityTransactionReqVo();
                liquidityTransaction.setErc20Address(dao.getErc20Token());
                liquidityTransaction.setErc20Name(dao.getErc20Name());
                liquidityTransaction.setErc20Symbol(dao.getDaoSymbol());
                liquidityTransaction.setTradeType(5);
                liquidityTransaction.setTransactionHash(tokenReceivedRecord.getTransactionHash());
                liquidityTransaction.setBlockTime(tokenReceivedRecord.getBlockTime());
                liquidityTransaction.setInTokenAmount(inTokenAmount);
                liquidityTransaction.setOutTokenAmount(outTokenAmount);
                liquidityTransaction.setEthAmount(outTokenAmount);
                liquidityTransaction.setUserAddress(tokenReceivedRecord.getFromAddress());

                UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
                userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
                userLiquidityStatisticsVo.setUserAddress(tokenReceivedRecord.getFromAddress());
                Result<Integer> result = protodaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[syncBurnTransaction] sync erc20Balance error for tokenReceivedRecord:{}",
                            JacksonUtil.obj2json(tokenReceivedRecord));
                    continue;
                }

                result = protodaoDexService.dexTransactionAdd(liquidityTransaction);
                if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                    tokenReceivedRecordList1.forEach(v -> v.setSyncDex(1));
                    tokenReceivedRecordService.updateBatchById(tokenReceivedRecordList1);
                    BigDecimal burnAmount = tokenReceivedRecordService.selectDaoBurnAmount(dao.getProjectId());
                    if (burnAmount != null
                            && (dao.getBurnAmount() == null || !dao.getBurnAmount().equals(burnAmount))) {
                        log.info("[syncBurnTransaction] daoId:{} burn Amount from:{} to:{}", dao.getId(),
                                dao.getBurnAmount(), burnAmount);
                        Dao updateDao = new Dao();
                        updateDao.setId(dao.getId());
                        updateDao.setBurnAmount(burnAmount);
                        daoService.updateById(updateDao);
                    }
                }
            }

        }

        log.info("[syncBurnTransaction] ending...");
    }

    @Async
    @Scheduled(cron = "0/30 * * * * ? ")
    public void syncDexForErc20() {
        log.info("[syncDexForErc20] running...");
        List<Dao> daoList = daoService.syncDexForErc20();
        for (Dao dao : daoList) {
            if (dao.getDaoStatus() < 0) {
                log.info("[syncDexForErc20] daoStatus zero daoId:{}", dao.getId());
                continue;
            }
            Dao updateDao = new Dao();
            updateDao.setId(dao.getId());

            Erc20LiquidityReqVo erc20LiquidityReqVo = new Erc20LiquidityReqVo();
            erc20LiquidityReqVo.setDaoId(dao.getId());
            erc20LiquidityReqVo.setProjectId(dao.getProjectId());
            erc20LiquidityReqVo.setErc20Address(dao.getErc20Token());
            erc20LiquidityReqVo.setErc20Name(dao.getErc20Name());
            erc20LiquidityReqVo.setErc20Symbol(dao.getDaoSymbol());
            erc20LiquidityReqVo.setErc20BlockTime(dao.getBlockTime());
            erc20LiquidityReqVo.setErc721Address(dao.getErc721Token());
            erc20LiquidityReqVo.setDaoStatus(dao.getDaoStatus() > 1 ? 1 : 0);
            erc20LiquidityReqVo.setDaoVersion(dao.getDaoVersion());
            log.info("[syncDexForErc20] erc20LiquidityReqVo:{}", JacksonUtil.obj2json(erc20LiquidityReqVo));
            Result<Integer> result = protodaoDexService.dexErc20Add(erc20LiquidityReqVo);
            if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                if (dao.getBurnAmount() == null) {
                    BigDecimal burnAmount = tokenReceivedRecordService.selectDaoBurnAmount(dao.getProjectId());
                    if (burnAmount != null) {
                        log.info("[syncDexForErc20] daoId:{} burn Amount:{}", dao.getId(), burnAmount);
                        updateDao.setBurnAmount(burnAmount);
                    }
                }
                if (dao.getDaoReward() == null) {
                    DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
                    if (daoDrbStatistics != null && daoDrbStatistics.getDaoReward() != null) {
                        log.info("[syncDexForErc20] daoId:{} dao reward:{}", dao.getId(),
                                daoDrbStatistics.getDaoReward());
                        updateDao.setDaoReward(daoDrbStatistics.getDaoReward());
                    }
                }
                // dao assetPool
                if (dao.getDaoAssetPool() == null) {
//                    Result<String> resultBalance =
//                            subscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
//                    if (resultBalance.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
//                        log.info("[EthTransferedChainService]infura ethGetBalance return data:{}",
//                                resultBalance.getData());
//                        String balance = resultBalance.getData();
//                        String price = CommonUtil.hexToTenString(balance);
//                        if (StringUtils.isNotBlank(price)) {
//                            BigDecimal assetPool = new BigDecimal(price)
//                                    .divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR);
//                            log.info("[EthTransferedChainService]daoId:{} assetPool:{}", dao.getId(), assetPool);
//                            dao.setDaoAssetPool(assetPool);
//                        }
//                    } else {
//                        log.error("[EthTransferedChainService]infura ethGetBalance return data:{} daoId:{}",
//                                resultBalance.getData(), dao.getId());
//                    }
                    dao.setDaoAssetPool(commonService.getInputToken(dao));
                }

                updateDao.setSyncDex(0);
                daoService.updateById(updateDao);
            } else {
                log.error("[syncDexForErc20] resultDesc:{} erc20LiquidityReqVo:{}", result.getResultDesc(),
                        JacksonUtil.obj2json(erc20LiquidityReqVo));
                continue;
            }
            List<TokenReceivedRecord> tokenReceivedRecordList =
                    tokenReceivedRecordService.recordListByProjectId(dao.getProjectId());
            if (tokenReceivedRecordList.size() > 0) {
                for (TokenReceivedRecord tokenReceivedRecord : tokenReceivedRecordList) {
                    UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
                    userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
                    userLiquidityStatisticsVo.setUserAddress(tokenReceivedRecord.getReceiveAddress());
                    log.info("[syncDexForErc20] userLiquidityStatisticsVo:{}",
                            JacksonUtil.obj2json(userLiquidityStatisticsVo));
                    result = protodaoDexService.syncErc20Balance(userLiquidityStatisticsVo);
                    if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                        log.error("[syncDexForErc20] sync erc20Balance resultDesc:{} for tokenReceivedRecord:{}",
                                result.getResultDesc(), JacksonUtil.obj2json(tokenReceivedRecord));
                    }
                }
            }

        }

        log.info("[syncDexForErc20] ending...");
    }

    /**
     * 每小时同步一次dao资金池金额
     */
    @Async
    @Scheduled(cron = "1 0 0/1 * * ? ")
    public void syncDaoReward() {
        log.info("[syncDaoReward] running...");
        List<Dao> daoList = daoService.daoStarted();
        List<Dao> newDaoList = new ArrayList<>();
        for (Dao dao : daoList) {
            // update dao assetPool
//            Result<String> result = subscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
//            if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
//                log.info("[syncDaoReward]infura ethGetBalance return data:{}", result.getData());
//                String balance = result.getData();
//                String price = CommonUtil.hexToTenString(balance);
//                if (StringUtils.isNotBlank(price)) {
//                    BigDecimal assetPool = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR);
//                    if (dao.getDaoAssetPool() == null || !dao.getDaoAssetPool().equals(assetPool)) {
//                        log.info("[syncDaoReward]daoId:{} oldAssetPool:[{} newAssetPool:{}", dao.getId(),
//                                dao.getDaoAssetPool(), assetPool);
//                        Dao updateDao = new Dao();
//                        updateDao.setId(dao.getId());
//                        updateDao.setDaoAssetPool(assetPool);
//                        newDaoList.add(updateDao);
//                    }
//                }
//            } else {
//                log.error("[syncDaoReward]infura daoId:{}  ethGetBalance return data:{} ", dao.getId(),
//                        result.getData());
//            }
            Dao updateDao = new Dao();
            updateDao.setDaoAssetPool(commonService.getInputToken(dao));
            newDaoList.add(updateDao);

        }
        if (newDaoList.size() > 0) {
            log.info("[syncDaoReward] update size:{}", newDaoList.size());
            daoService.updateBatchById(newDaoList);
        }

        log.info("[syncDaoReward] ending...");
    }

//    @Async
//    @Scheduled(cron = "0 30 0/12 * * ? ")
//    public void daoRoyaltyFee() throws Exception {
//        log.info("[daoRoyaltyFee] running...");
//        String daoId = ProtoDaoConstant.royaltyFeeDao;
//        if (StringUtils.isBlank(daoId)) {
//            log.info("[daoRoyaltyFee] daoId is null");
//            return;
//        }
//        Dao dao = daoService.getById(daoId);
//        if (dao == null) {
//            log.info("[daoRoyaltyFee] dao is null");
//            return;
//        }
//        // 获取二次交易金额
//        HttpHeaders headers = new HttpHeaders();
//        headers.set("user-agent", "Chrome/83.0.4103.116");
//        HttpEntity<String> httpEntity = new HttpEntity<>(headers);
//        String queryUrl = "https://api.dune.com/api/v1/query/2665201/results?api_key=kicC9NiRCVIxSyXX0PpiOnUb8x2BUbjs";
//        ResponseEntity<String> responseEntity =
//                REST_TEMPLATE.exchange(queryUrl, HttpMethod.GET, httpEntity, String.class);
//        BigDecimal royaltyFeeIncome = BigDecimal.ZERO;
//        if (responseEntity.getStatusCode() != HttpStatus.OK) {
//            log.error("[daoRoyaltyFee] error queryUrl:{} error:{}", queryUrl,
//                    responseEntity.getBody());
//            return;
//        } else {
//            String resBody = responseEntity.getBody();
//            log.info("[daoRoyaltyFee] daoId:{} resBody:{}", dao.getId(), resBody);
//            Map<String, Object> resMap = JacksonUtil.json2map(resBody);
//            Object result = resMap.get("result");
//            Map<String, Object> resultMap = JacksonUtil.json2map(JacksonUtil.obj2json(result));
//            Object rows = resultMap.get("rows");
//            List<Map> rowsMapList = JacksonUtil.json2list(JacksonUtil.obj2json(rows), Map.class);
//            String eth = String.valueOf(rowsMapList.get(0).get("eth"));
//            BigDecimal ethBigDecimal = new BigDecimal(eth);
//            if (dao.getRoyaltyFee() != null) {
//                royaltyFeeIncome = ethBigDecimal.multiply(new BigDecimal(dao.getRoyaltyFee()).divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))).setScale(4, RoundingMode.HALF_UP);
//            }
//
//        }
//
//        Dao updateDao = new Dao();
//        updateDao.setId(dao.getId());
//        updateDao.setRoyaltyFeeIncome(royaltyFeeIncome);
//        log.info("[daoRoyaltyFee] daoId:{} royaltyFeeIncome:{}", dao.getId(), royaltyFeeIncome);
//        daoService.updateById(updateDao);
//
//    }

    //    @Async
    //    @Scheduled(cron = "0/30 * * * * ? ")
    @Deprecated
    public void syncDaoStart() {
        log.info("[syncDaoStart] running...");
        List<Dao> daoList = daoService.syncDaoStatus();
        if (daoList != null && daoList.size() > 0) {
            long time = System.currentTimeMillis() / 1000;
            for (Dao dao : daoList) {
                if (time - Long.parseLong(dao.getBlockTime()) > 35) {
                    Dao updateDao = new Dao();
                    updateDao.setId(dao.getId());
                    updateDao.setDaoStatus(DaoStatusEnum.NOT_STARTED.getStatus());
                    if (dao.getDaoStartDrb() != null
                            && Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).compareTo(dao.getDaoStartDrb()) >= 0) {
                        updateDao.setDaoStatus(DaoStatusEnum.STARTED.getStatus());

                        if (dao.getRoyaltyTokenLotteryMode() != null && dao.getRoyaltyTokenLotteryMode() == 1) {
                            DaoDrbStatistics daoDrbStatistics1 = new DaoDrbStatistics();
                            daoDrbStatistics1.setFloorPrice(dao.getDaoFloorPrice());
                            daoDrbStatistics1.setDaoId(dao.getId());
                            daoDrbStatistics1.setStatus(StatisticsStatusEnum.WJS.getStatus());
                            daoDrbStatistics1.setDrbVol(BigDecimal.ZERO);
                            daoDrbStatistics1.setDrbVolExTax(BigDecimal.ZERO);

                            daoDrbStatistics1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                            log.info("[syncDaoStart] daoId:{} start add DaoDrbStatistics daoStatus:{}", dao.getId(), updateDao.getDaoStatus());
                            daoDrbStatisticsService.updateDaoAndDaoDrbStatistics(daoDrbStatistics1, updateDao);
                            continue;
                        }
                    }
                    log.info("[syncDaoStart] daoId:{} daoStatus:{}", dao.getId(), updateDao.getDaoStatus());
                    daoService.updateById(updateDao);
                }
            }
        }
    }

    /**
     * 30秒同步一次dao的flow流水信息
     */
    //@Async
    //@Scheduled(cron = "0/35 * * * * ? ")
    public void syncDaoFlow() {
        log.info("[syncDaoFlow] running...");
        List<Dao> daoList = daoService.daoStarted();
        List<Dao> newDaoList = new ArrayList<>();
        for (Dao dao : daoList) {
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao())) {
                continue;
            }
            // dao的flow流水信息
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.GET_TURNOVER.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.GET_TURNOVER.getMethodAddress() + dao.getProjectId());
            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            log.info(
                    "[syncDaoFlow]getTurnover daoId:{} infura return data:{}",
                    dao.getId(), result.getData());
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error(
                        "[syncDaoFlow] getTurnover error result:{} daoId:{}",
                        result.getResultDesc(), dao.getId());
                continue;
            }
            String daoFlow = CommonUtil.hexToTenString(result.getData());
            if (StringUtils.isNotBlank(daoFlow)) {
                BigDecimal daoFlowBigDecimal = new BigDecimal(daoFlow).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);
                if (dao.getDaoFlow() == null || !dao.getDaoFlow().equals(daoFlowBigDecimal)) {
                    log.info("[syncDaoFlow] changed daoId:{} daoFlow:{} newFlow:{}", dao.getId(), dao.getDaoFlow(), daoFlowBigDecimal);
//                    dao.setDaoFlow(daoFlowBigDecimal);
                    Dao updateDao = new Dao();
                    updateDao.setId(dao.getId());
                    updateDao.setDaoFlow(daoFlowBigDecimal);

                    newDaoList.add(updateDao);
                }
            }


        }
        if (newDaoList.size() > 0) {
            log.info("[syncDaoFlow] update size:{}", newDaoList.size());
            daoService.updateBatchById(newDaoList);
        }

        log.info("[syncDaoFlow] ending...");
    }

    /**
     * 5分钟同步一次dao的erc20的holders信息
     */
    @Async
    @Scheduled(cron = "0 0/10 * * * ? ")
    public void syncDaoErc20Holders() {
        log.info("[syncDaoErc20Holders] running...");
        List<Dao> daoList = daoService.daoStarted();
        List<Dao> newDaoList = new ArrayList<>();
        for (Dao dao : daoList) {
            // if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao())) {
            // continue;
            // }

            try {
                Thread.sleep(1000);
                Integer tokenHolders = commonService.tokenHolders(dao.getErc20Token());
                if (!tokenHolders.equals(dao.getTokenHolders())) {
                    Dao updateDao = new Dao();
                    updateDao.setId(dao.getId());
                    //更新dao的token的holders数量
                    updateDao.setTokenHolders(tokenHolders);
                    newDaoList.add(updateDao);
                }
            } catch (InterruptedException e) {
                log.info("[syncDaoErc20Holders] InterruptedException daoId:{} ", dao.getId());
            }

        }
        if (newDaoList.size() > 0) {
            log.info("[syncDaoErc20Holders] update size:{}", newDaoList.size());
            daoService.updateBatchById(newDaoList);
        }

        log.info("[syncDaoErc20Holders] ending...");
    }

    /**
     * 30秒同步一次dao的创建work信息
     */
    @Async
    @Scheduled(cron = "0/30 * * * * ? ")
    public void syncDaoWorkGenerate() {
        log.info("[syncDaoWorkGenerate] running...");
        List<Dao> daoList = daoService.protoDaoGenerateList();
        long currentTime = System.currentTimeMillis() / 1000;
        log.info("[syncDaoWorkGenerate] currentTime:{}", currentTime);
        if (daoList.isEmpty()) {
            log.info("[syncDaoWorkGenerate] fast end...");
            return;
        }
        List<Dao> newDaoList = new ArrayList<>();

        for (Dao dao : daoList) {
            //1674218183 1694598139
//            if (currentTime - Long.parseLong(dao.getBlockTime()) < 120) {
//                continue;
//            }
            Work work = workService.selectLastGenerateWork(dao.getId());
//            if(work != null && work.getWorkStatus().equals(WorkStatusEnum.NOT_CAST.getStatus())){
//                log.info("[syncDaoWorkGenerate] NOT_CAST daoId:{}", dao.getId());
//                continue;
//            }
            Integer workNum = (work == null || work.getWorkNumber() == null) ? 1 : work.getWorkStatus().equals(WorkStatusEnum.NOT_CAST.getStatus()) ? work.getWorkNumber() : work.getWorkNumber() + 1;

            if (StringUtils.isBlank(dao.getDaoUri())) {
                continue;
            }

            String daoUriHash = dao.getDaoUri().substring(dao.getDaoUri().lastIndexOf("/") + 1, dao.getDaoUri().lastIndexOf("."));
            String canvasId = CommonUtil.removeHexPrefixIfExists(Hash.sha3(daoUriHash + dao.getOwnerAddress()));
            Canvas canvas = canvasService.selectCanvasDetailByCanvasId(canvasId);
            if (canvas == null) {
                log.error("[syncDaoWorkGenerate] add work canvas is null daoId:{}", dao.getId());
                continue;
            }
            if (commonService.addWork(dao, canvas, workNum)) {
                log.info("[syncDaoWorkGenerate] add work daoId:{} workNum:{}", dao.getId(), workNum);
                Dao updateDao = new Dao();
                updateDao.setId(dao.getId());
                updateDao.setAddWork(1);
                newDaoList.add(updateDao);
//                newDaoList.add(dao);
                if (StringUtils.isBlank(dao.getDaoWorkUrl())) {
                    try {
                        log.info("[syncDaoWorkGenerate] addWork success daoId:{}", dao.getId());
                        //处理logo转work图片地址及高度／背景色 hash等信息 抽象为一个方法

                        String filePath = String.format(ProtoDaoConstant.workImageDaoLogoUrl, dao.getDaoNumber()) + File.separatorChar + "0.png";
                        ImageUtil.imageAddText(ProtoDaoConstant.workImageDefaultUrl, filePath, dao.getDaoName(), null, "png");

                        File imageFile = new File(filePath);
                        s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, imageFile,
                                dao.getDaoNumber() + "_0.png", false);

                        dao.setWorkUrlSuffix(".png");
                        ImageUtil.HeightAndBgColor heightAndBgColor = ImageUtil.getImageRgb(imageFile);
                        if (heightAndBgColor != null) {
                            dao.setHeight(heightAndBgColor.getHeight());
                            dao.setColor(heightAndBgColor.getBgColor());
                        }
                        String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                        String daoLogoWorkName = dao.getDaoNumber() + "_0.png";
                        String daoWorkUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + daoLogoWorkName;
                        dao.setDaoWorkUrl(daoWorkUrl);
                        String workHash = ImageUtil.getMD5(imageFile);
                        dao.setWorkHash(workHash);

                    } catch (Exception e) {
                        log.error("[syncDaoWorkGenerate] addWork fail daoId:{} e", dao.getId(), e);
                        dao.setAddWork(0);
                    }
                }

            }
        }
        if (newDaoList.size() > 0) {
            log.info("[syncDaoWorkGenerate] update size:{}", newDaoList.size());
            daoService.updateBatchById(newDaoList);
        }

        log.info("[syncDaoWorkGenerate] ending...");
    }

    /**
     * 每日零点计算前一天到今天的收入和支出
     * //支出只有burn交易的数量 查询昨天一天burn交易的eth数量
     * //收入 = 资金池变化量 + 支出
     */
    @Async
    @Scheduled(cron = "1 5 0,1 * * ? ")
    public void daoDailyStatisticsCalculate() {

        Timestamp today = DateUtil.getBeginOfToday();
//        long todayBeginHour = DateUtil.getTimestampAfterDay(today, 0).getTime() / 1000;
        long yesterdayBeginHour = DateUtil.getTimestampAfterDay(today, -1).getTime() / 1000;
        long beforeYesterdayBeginHour = DateUtil.getTimestampAfterDay(today, -2).getTime() / 1000;
        log.info("[daoDailyStatisticsCalculate] start yesterdayBeginHour:{}  beforeYesterdayBeginHour:{}", yesterdayBeginHour, beforeYesterdayBeginHour);
        List<Integer> daoIds = new ArrayList<>();
        List<DaoDrbStatistics> daoDrbStatisticsUpdateList = new ArrayList<>();
        List<DaoDailyStatistics> daoDailyStatisticsList = new ArrayList<>();
        Map<Integer, DaoDailyStatistics> beforeDaoDailyStatisticsMap = new HashMap<>();

        Map<Integer, String> daoMap = new HashMap<>();

        //查询所有开始的dao 查询已经结束的，但是在上一次计算中还需要计算的dao
        List<DaoDailyStatistics> incompleteStatusStatisticsList = daoDailyStatisticsService.selectDaoDailyIncompleteStatus(yesterdayBeginHour);
        if (!incompleteStatusStatisticsList.isEmpty()) {
            daoIds = incompleteStatusStatisticsList.stream().map(DaoDailyStatistics::getDaoId).collect(Collectors.toList());
        }
        List<Dao> daoList = daoService.daoStarted();

//        if (!daoList.isEmpty()) {
//            //前天的记录
//            List<DaoDailyStatistics> beforeDaoDailyStatisticsList = daoDailyStatisticsService.selectDaoDailyCompleteStatus(beforeYesterdayBeginHour);
//            if (!beforeDaoDailyStatisticsList.isEmpty()) {
//                beforeDaoDailyStatisticsMap = beforeDaoDailyStatisticsList.stream().collect(Collectors.toMap(DaoDailyStatistics::getDaoId, v -> v, (v1, v2) -> v2));
//            }
//        }
        for (Dao dao : daoList) {
            try {
                if (StringUtils.isBlank(daoMap.get(dao.getId()))) {
                    //由于calculateDaoDailyStatisticsNew查询不能太频繁，所以这里停留5秒
                    Thread.sleep(1000 * 5);
                    DaoDailyStatistics yesterdayDaoDailyStatistic = daoDailyStatisticsService.selectDaoDailyCompleteByDaoId(dao.getId(), yesterdayBeginHour);
                    if (yesterdayDaoDailyStatistic != null) {
                        log.info("[daoDailyStatisticsCalculate] daoId:{} had success", dao.getId());
                        continue;
                    }
                    if (daoIds.isEmpty() || !daoIds.contains(dao.getId())) {
                        //前天的记录
//                    DaoDailyStatistics beforeYesterdayDaoDailyStatistic = beforeDaoDailyStatisticsMap.get(dao.getId());

//                DaoDailyStatistics daoDailyStatistics = calculateDaoDailyStatistics(beforeYesterdayDaoDailyStatistic, dao, null, daoDrbStatisticsUpdateList, todayBeginHour, yesterdayBeginHour);
                        DaoDailyStatistics daoDailyStatistics = calculateDaoDailyStatisticsNew(dao, null, yesterdayBeginHour);
                        daoMap.put(dao.getId(), dao.getProjectId());
                        daoDailyStatisticsList.add(daoDailyStatistics);
                    }
                }
            } catch (Exception e) {
                log.error("[daoDailyStatisticsCalculate] daoId:{} had error:", dao.getId(), e);
            }

        }


        //结束的dao会插入一条记录。也需要判断是否在前面已经计算过了。

        if (!incompleteStatusStatisticsList.isEmpty()) {
            for (DaoDailyStatistics daoDailyStatistics : incompleteStatusStatisticsList) {
                try {
                    if (StringUtils.isBlank(daoMap.get(daoDailyStatistics.getDaoId()))) {
                        //由于calculateDaoDailyStatisticsNew查询不能太频繁，所以这里停留5秒
                        Thread.sleep(1000 * 5);
                        //查询dao
                        Dao dao = daoService.getById(daoDailyStatistics.getDaoId());
                        //查询前天的记录
//                    DaoDailyStatistics beforeYesterdayDaoDailyStatistic = daoDailyStatisticsService.selectDaoDailyCompleteByDaoId(daoDailyStatistics.getDaoId(), beforeYesterdayBeginHour);
//                daoDailyStatistics = calculateDaoDailyStatistics(beforeYesterdayDaoDailyStatistic, dao, daoDailyStatistics, daoDrbStatisticsUpdateList, todayBeginHour, yesterdayBeginHour);
                        daoDailyStatistics = calculateDaoDailyStatisticsNew(dao, daoDailyStatistics, yesterdayBeginHour);
                        daoMap.put(dao.getId(), dao.getProjectId());
                        daoDailyStatisticsList.add(daoDailyStatistics);
                    }
                } catch (Exception e) {
                    log.error("[daoDailyStatisticsCalculate] incompleteStatusStatisticsList daoId:{} had error:", daoDailyStatistics.getDaoId(), e);
                }
            }
        }

        int i = daoDailyStatisticsService.insertDaoDailyStatisticsAndUpdateDaoDrbStatistics(daoDrbStatisticsUpdateList, daoDailyStatisticsList);

        log.info("[daoDailyStatisticsCalculate] update i:{}", i);
    }

    //========================================private============================================================//

    /**
     * 1分钟同步一次work锁定状态
     */
    @Async
    @Scheduled(cron = "0 0/1 * * * ? ")
    public void workLockStatusSync() {
        log.info("[workLockStatusSync] running...");
        Result<String> resultBlockNum = subscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
        if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[workLockStatusSync] ethGetBlockNumber error:{}", resultBlockNum.getResultDesc());
            return;
        }
        List<Work> workList = workService.selectWorksIsLockStatus(CommonUtil.hexToTenString(resultBlockNum.getData()));

        for (Work work : workList) {
            Dao dao = daoService.getById(work.getDaoId());
            if (dao != null) {
                // 如果查询出合约上已解锁，那么更新状态为已解锁
                if (!commonService.getTopUpNftLockedStatus(dao, work)) {
                    work.setLockDurationBlock(null);
                    work.setLockStartBlock(null);
                    work.setLockStatus(WorkLockStatusEnum.NOT_LOCK.getStatus());
                    workService.updateById(work);
                }
            }
        }
        log.info("[workLockStatusSync] ending...");
    }

    /**
     * 计算dao当日统计信息
     *
     * @param beforeYesterdayDaoDailyStatistic 前天的记录
     * @param dao                              当天dao
     * @param daoDailyStatistic                需要返回的记录
     * @param daoDrbStatisticsUpdateList       返回需要更新recordTime的列表
     * @param todayBeginHour                   今天零点的时间戳
     * @param yesterdayBeginHour               昨天零点时间戳
     * @return DaoDailyStatistics 当日的记录
     */
    private DaoDailyStatistics calculateDaoDailyStatistics(DaoDailyStatistics beforeYesterdayDaoDailyStatistic, Dao dao,
                                                           DaoDailyStatistics daoDailyStatistic, List<DaoDrbStatistics> daoDrbStatisticsUpdateList,
                                                           long todayBeginHour, long yesterdayBeginHour) {
        BigDecimal beforeYesterdayEthBalance = beforeYesterdayDaoDailyStatistic == null ? BigDecimal.ZERO : beforeYesterdayDaoDailyStatistic.getAssetPoolEthTotalAmount();
        BigDecimal beforeYesterdayTokenBalance = beforeYesterdayDaoDailyStatistic == null ? BigDecimal.ZERO : beforeYesterdayDaoDailyStatistic.getAssetPoolTokenTotalAmount();
        //生成今天的记录
        if (daoDailyStatistic == null) {
            daoDailyStatistic = new DaoDailyStatistics();
        }
        daoDailyStatistic.setDaoId(dao.getId());
        daoDailyStatistic.setProjectId(dao.getProjectId());
        //查询当前dao的assetPool的eth余额和token余额
        // BigDecimal ethBalance = commonService.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals());
        BigDecimal ethBalance = commonService.getInputToken(dao);
        BigDecimal tokenBalance = commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());
        log.info("[calculateDaoDailyStatistics] daoId:{} logTime:{} ethBalance:{} tokenBalance:{}", dao.getId(), yesterdayBeginHour, ethBalance, tokenBalance);


        //把已经计算完的拿出来取支出值，未计算完成的更改recordTime值为下一天
        List<DaoDrbStatistics> daoDrbStatisticsList = daoDrbStatisticsService.selectByDaoIdAndRecordTime(dao.getId(), yesterdayBeginHour);
        BigDecimal costToken = BigDecimal.ZERO;
        BigDecimal costEth = BigDecimal.ZERO;
        if (!daoDrbStatisticsList.isEmpty()) {
            costToken = daoDrbStatisticsList.stream().filter(v -> StatisticsStatusEnum.JSWC.getStatus().equals(v.getStatus())).map(DaoDrbStatistics::getAssetPoolTokenCost).reduce(BigDecimal.ZERO, BigDecimal::add);
            costEth = daoDrbStatisticsList.stream().filter(v -> StatisticsStatusEnum.JSWC.getStatus().equals(v.getStatus())).map(DaoDrbStatistics::getAssetPoolEthCost).reduce(BigDecimal.ZERO, BigDecimal::add);
            //未计算完成的更改时间到下一天
            List<DaoDrbStatistics> daoDrbStatisticsUpdates = daoDrbStatisticsList.stream().filter(v -> !StatisticsStatusEnum.JSWC.getStatus().equals(v.getStatus())).collect(Collectors.toList());
            if (!daoDrbStatisticsUpdates.isEmpty()) {
                daoDrbStatisticsUpdates.forEach(v -> {
                    log.info("[calculateDaoDailyStatistics]-updateDaoDrbStatisticsRecordTime daoId:{} DaoDrbStatisticsId:{} updateRecordTime:{} ", dao.getId(), v.getId(), todayBeginHour);
                    v.setRecordTime(todayBeginHour);
                });
                daoDrbStatisticsUpdateList.addAll(daoDrbStatisticsUpdates);
            }
        }

        daoDailyStatistic.setAssetPoolTokenTotalAmount(tokenBalance);
        //收入 = 变化量 + 支出
        daoDailyStatistic.setAssetPoolTokenIncome(tokenBalance.subtract(beforeYesterdayTokenBalance).add(costToken));
        //支出只有一个途径就是资产出块 包括两种情况，一种是dao的每个区块在一天之内的，还有一种是每个区块在两天内的  查询每个dao的window的按天查询，
        daoDailyStatistic.setAssetPoolTokenCost(costToken);
        daoDailyStatistic.setAssetPoolTokenVariation(tokenBalance.subtract(beforeYesterdayTokenBalance));


        daoDailyStatistic.setAssetPoolEthTotalAmount(ethBalance);
        daoDailyStatistic.setAssetPoolEthIncome(ethBalance.subtract(beforeYesterdayEthBalance).add(costEth));
        daoDailyStatistic.setAssetPoolEthCost(costEth);
        daoDailyStatistic.setAssetPoolEthVariation(ethBalance.subtract(beforeYesterdayEthBalance));
        daoDailyStatistic.setStatus(StatisticsStatusEnum.JSWC.getStatus());
        daoDailyStatistic.setRecordTime(yesterdayBeginHour);

        log.info("[calculateDaoDailyStatistics]-daoDailyStatistic daoId:{} logTime:{} daoDailyStatistic:{} ", dao.getId(), yesterdayBeginHour, JacksonUtil.obj2json(daoDailyStatistic));
        return daoDailyStatistic;
    }

    /**
     * 计算dao当日统计信息
     *
     * @param dao                当天dao
     * @param daoDailyStatistic  需要返回的记录
     * @param yesterdayBeginHour 昨天零点时间戳
     * @return DaoDailyStatistics 当日的记录
     */
    private DaoDailyStatistics calculateDaoDailyStatisticsNew(Dao dao, DaoDailyStatistics daoDailyStatistic, long yesterdayBeginHour) {
        //生成今天的记录
        if (daoDailyStatistic == null) {
            daoDailyStatistic = new DaoDailyStatistics();
        }
        daoDailyStatistic.setDaoId(dao.getId());
        daoDailyStatistic.setProjectId(dao.getProjectId());
        //查询当前dao的assetPool的eth余额和token余额
        // BigDecimal ethBalance = commonService.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals());
        BigDecimal ethBalance = commonService.getInputToken(dao);
        BigDecimal tokenBalance = commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());
        log.info("[calculateDaoDailyStatisticsNew] daoId:{} logTime:{} ethBalance:{} tokenBalance:{}", dao.getId(), yesterdayBeginHour, ethBalance, tokenBalance);


        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        Date today = new Date();
        Date yesterday = DateUtil.addDay(today, -1);
        String todayStr = sdf.format(today);
        String yesterdayStr = sdf.format(yesterday);


        BigDecimal incomeToken = commonService.searchTokenIncome(yesterdayStr, todayStr, dao.getFeePool(), dao.getErc20Token());
        BigDecimal costToken = commonService.searchTokeCost(yesterdayStr, todayStr, dao.getFeePool(), dao.getErc20Token());
        BigDecimal incomeEth = commonService.searchEthIncome(yesterdayStr, todayStr, dao.getFeePool());
        BigDecimal costEth = commonService.searchEthCost(yesterdayStr, todayStr, dao.getFeePool());

        daoDailyStatistic.setAssetPoolTokenTotalAmount(tokenBalance);
        //收入 = 变化量 + 支出
        daoDailyStatistic.setAssetPoolTokenIncome(incomeToken);
        //支出只有一个途径就是资产出块 包括两种情况，一种是dao的每个区块在一天之内的，还有一种是每个区块在两天内的  查询每个dao的window的按天查询，
        daoDailyStatistic.setAssetPoolTokenCost(costToken);
        daoDailyStatistic.setAssetPoolTokenVariation(incomeToken.subtract(costToken));


        daoDailyStatistic.setAssetPoolEthTotalAmount(ethBalance);
        daoDailyStatistic.setAssetPoolEthIncome(incomeEth);
        daoDailyStatistic.setAssetPoolEthCost(costEth);
        daoDailyStatistic.setAssetPoolEthVariation(incomeEth.subtract(costEth));
        daoDailyStatistic.setStatus(StatisticsStatusEnum.JSWC.getStatus());
        daoDailyStatistic.setRecordTime(yesterdayBeginHour);

        log.info("[calculateDaoDailyStatisticsNew]-daoDailyStatistic daoId:{} logTime:{} daoDailyStatistic:{} ", dao.getId(), yesterdayBeginHour, JacksonUtil.obj2json(daoDailyStatistic));
        return daoDailyStatistic;
    }

    /**
     * 定时任务，补偿dao相关的订阅事件
     */
    @Async
    @Scheduled(cron = "0 0/10 * * * ? ")
    public void subscribeStatusDao() {
        log.info("[subscribeStatusDao] running...");
        List<Subscribe> subscribeList = subscribeService.selectAll();
        List<Dao> daoList = daoService.daoStarted();


        log.info("[subscribeStatusDao] subscribeList size:{}", subscribeList.size());

        Map<String, String> contractMaps = subscribeList.stream()
                .collect(Collectors.toMap(Subscribe::getContractAddress, Subscribe::getTopics, (v1, v2) -> v1));

        Map<String, Dao> erc20Map = daoList.stream().filter(v -> contractMaps.get(v.getErc20Token()) == null)
                .collect(Collectors.toMap(Dao::getErc20Token, v -> v, (v1, v2) -> v1));
        Map<String, Dao> erc21Map = daoList.stream().filter(v -> contractMaps.get(v.getErc721Token()) == null)
                .collect(Collectors.toMap(Dao::getErc721Token, v -> v, (v1, v2) -> v1));

        Subscribe subscribe1 =
                subscribeList.stream().filter(v -> TradeTypeEnum.D4A_MINTNFT.getType().equals(v.getTradeType())
                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());

        for (String erc20 : erc20Map.keySet()) {
            Dao dao = erc20Map.get(erc20);
            // 异步订阅
            Subscribe subscribe2 = new Subscribe();
            subscribe2.setContractAddress(dao.getErc20Token());
            subscribe2.setTopics(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
            subscribe2.setFromBlock(dao.getBlockNumber());
            subscribe2.setReceiveAddress(subscribe1.getReceiveAddress());
            subscribe2.setTradeType(TradeTypeEnum.TRANSFER_ERC20.getType());
            subscribe2.setOrderInit(subscribeList.size() + 2);
            subscribe2.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

            SubscribeRequestDto subscribeRequestDto2 = new SubscribeRequestDto();
            subscribeRequestDto2.setAddress(dao.getErc20Token());
            subscribeRequestDto2.setFromBlock(dao.getBlockNumber());
            subscribeRequestDto2.setNetwork(ProtoDaoConstant.netWork);
            subscribeRequestDto2
                    .setTopics(Collections.singletonList(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress()));
            subscribeRequestDto2.setNoticeType(SubscriberTypeEnum.BATCH_TRAN.getType());
            subscribeRequestDto2.setNoticeUrl(subscribe1.getReceiveAddress());
            subscribeRequestDto2.setAppName(ProtoDaoConstant.netWork + "-" + ProtoDaoConstant.appName);
            subscribeRequestDto2.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
            try {
                Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto2);
                if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[subscribeStatusDao] subscripe erc20 retry error erc20:{} resultDesc:{}",
                            erc20, subResult.getResultDesc());
                    subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
                } else {
                    subscribe2.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribe2.setFilterId(subResult.getData());
                }
            } catch (Exception e) {
                log.error("[subscribeStatusDao] subscripe erc20 retry error erc20:{} e:{}", erc20, e);
                subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
            }
            subscribeService.save(subscribe2);
        }


        for (String erc721 : erc21Map.keySet()) {
            Dao dao = erc21Map.get(erc721);
            // 异步订阅
            Subscribe subscribe = new Subscribe();
            subscribe.setContractAddress(dao.getErc721Token());
            subscribe.setTopics(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
            subscribe.setFromBlock(dao.getBlockNumber());
            subscribe.setReceiveAddress(subscribe1.getReceiveAddress());
            subscribe.setTradeType(TradeTypeEnum.TRANSFER.getType());
            subscribe.setOrderInit(subscribeList.size() + 1);
            subscribe.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

            SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
            subscribeRequestDto.setAddress(dao.getErc721Token());
            subscribeRequestDto.setFromBlock(dao.getBlockNumber());
            subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
            subscribeRequestDto
                    .setTopics(Collections.singletonList(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress()));
            subscribeRequestDto.setNoticeType(SubscriberTypeEnum.EVENT.getType());
            subscribeRequestDto.setNoticeUrl(subscribe1.getReceiveAddress());
            subscribeRequestDto.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
            subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + ProtoDaoConstant.appName);
            try {
                Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto);
                if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[subscribeStatusDao] subscripe erc21 retry error erc721:{} resultDesc:{}",
                            erc721, subResult.getResultDesc());
                    subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
                } else {
                    subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribe.setFilterId(subResult.getData());
                }
            } catch (Exception e) {
                log.error("[subscribeStatusDao] subscripe erc21 retry error erc721:{} e:{}", erc721, e);
                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
            }
            subscribeService.save(subscribe);

        }


    }

    /**
     * 定时任务，补偿dao的GET_DAO_CURRENT_ROUND相关的订阅事件
     */
    @Async
    @Scheduled(cron = "0 0/2 * * * ? ")
    public void subscribeDaoCurrentRount() {
        log.info("[subscribeDaoCurrentRount] running...");
        List<Subscribe> subscribeList = subscribeService.selectAll();
        List<Dao> allDaoList = daoService.listAll();


        log.info("[subscribeDaoCurrentRount] subscribeList size:{}", subscribeList.size());


        Map<String, String> projectIdMaps = subscribeList.stream().filter(v -> v.getTopics().contains(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress()))
                .collect(Collectors.toMap(v -> v.getTopics().replace(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress(), ""), Subscribe::getTopics, (v1, v2) -> v1));

        Map<String, Dao> projectIdMap = allDaoList.stream().filter(v -> projectIdMaps.get(v.getProjectId()) == null)
                .collect(Collectors.toMap(Dao::getProjectId, v -> v, (v1, v2) -> v1));

        Subscribe subscribex =
                subscribeList.stream().filter(v -> TradeTypeEnum.CURRENT_ROUND.getType().equals(v.getTradeType())
                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());

        for (String projectId : projectIdMap.keySet()) {
            Dao dao = projectIdMap.get(projectId);
            // 异步订阅
            Subscribe subscribe = new Subscribe();
            subscribe.setContractAddress(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getContractAddress());
            subscribe.setTopics(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress() + dao.getProjectId());
            subscribe.setFromBlock(dao.getBlockNumber());
            subscribe.setReceiveAddress(subscribex.getReceiveAddress());
            subscribe.setTradeType(TradeTypeEnum.getDaoCurrentRound.getType());
            subscribe.setOrderInit(subscribeList.size() + 1);
            subscribe.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

            SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
            subscribeRequestDto.setAddress(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getContractAddress());
            subscribeRequestDto.setFromBlock(dao.getBlockNumber());
            subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
            subscribeRequestDto
                    .setTopics(Collections.singletonList(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress() + dao.getProjectId()));
            subscribeRequestDto.setNoticeType(SubscriberTypeEnum.VALUE.getType());
            subscribeRequestDto.setNoticeUrl(subscribex.getReceiveAddress());
            subscribeRequestDto.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
            subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + ProtoDaoConstant.appName);
            try {
                Result<String> subResult = subscriptionService.subscripe(subscribeRequestDto);
                if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[GET_DAO_CURRENT_ROUND] subscripe PROJECT_TRANSFER retry error projectId:{} resultDesc:{}",
                            projectId, subResult.getResultDesc());
                    subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
                } else {
                    subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribe.setFilterId(subResult.getData());
                    log.info("[GET_DAO_CURRENT_ROUND] subscripe daoId:{} ", dao.getId());
                }
            } catch (Exception e) {
                log.error("[GET_DAO_CURRENT_ROUND] subscripe PROJECT_TRANSFER retry error projectId:{} e:{}", projectId, e);
                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
            }
            subscribeService.save(subscribe);

        }
    }

}
