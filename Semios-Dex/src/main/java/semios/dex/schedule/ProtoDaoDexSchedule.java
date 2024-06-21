package semios.dex.schedule;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import semios.dex.model.dto.common.Dao4ArtDexConstant;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.common.ResultList;
import semios.dex.model.dto.feign.DaoErc20Dto;
import semios.dex.model.dto.request.InfuraCallRequestDto;
import semios.dex.model.dto.request.SubscribeRequestDto;
import semios.dex.model.entity.*;
import semios.dex.model.enums.*;
import semios.dex.service.*;
import semios.dex.service.feign.IDao4ArtService;
import semios.dex.service.feign.IProtoDaoService;
import semios.dex.service.feign.ISubscriptionService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.DateUtil;
import semios.dex.utils.JacksonUtil;
import semios.dex.utils.ProtoDaoDexCommonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

/**
 * @description: gas
 * @author: xiangbin
 * @create: 2022-04-24 13:48
 **/
@Slf4j
@Configuration
@EnableScheduling
@EnableAsync
public class ProtoDaoDexSchedule {


    @Autowired
    private IUserLiquidityStatisticsService userLiquidityStatisticsService;

    @Autowired
    private ISubscriptionService subscriptionService;

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ILiquidityPriceRecordService liquidityPriceRecordService;

    @Autowired
    private ILiquidityDailyStatisticsService liquidityDailyStatisticsService;

    @Autowired
    private ISubscribeService subscribeService;

    @Autowired
    private IDao4ArtService dao4ArtService;

    @Autowired
    private IProtoDaoService protoDaoService;

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
                    log.error("[Dao4ArtSchedule]tradeTypeEnum is null subId:{}", subscribe.getId());
                    continue;
                }
                SubscriberTypeEnum subscriberTypeEnum = tradeTypeEnum.getSubscriberTypeEnum();
                if (subscriberTypeEnum == null) {
                    log.error("[Dao4ArtSchedule] subscriberTypeEnum is null subId:{}", subscribe.getId());
                    continue;
                }
                // 值类型的
                if (tradeTypeEnum.getLocal() && subscriberTypeEnum.getType().equals(SubscriberTypeEnum.VALUE.getType())) {
                    // 本地存储的值
                    String value = ProtoDaoDexCommonUtil.ethCall(subscriptionService, Dao4ArtDexConstant.netWork, subscribe.getContractAddress(), subscribe.getTopics());
                    log.info("Dao4ArtSchedule return subscribe:{}, value:{}", subscribe.getTradeType(), value);
                    if (StringUtils.isBlank(value)) {
                        log.error("[Dao4ArtSchedule]tradeTypeEnum value is null subId:{}", subscribe.getId());
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
                        log.error("[Dao4ArtSchedule] subscripeStatus error filterId:{} status resultDesc:{}", subscribe.getFilterId(), result.getResultDesc());
                        continue;
                    }
                    if (result.getData().equalsIgnoreCase("1")) {// 0-关闭 1-开启 开启则不用管，如果已经关闭了，则查询上次通知的区块，然后重新订阅
                        log.info("[Dao4ArtSchedule] subId:{} status is ok!", subscribe.getId());
                        subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                        subscribeService.updateById(subscribe);
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
                    log.error("[Dao4ArtSchedule] subscripe retry error filterId:{} status resultDesc:{}", subscribe.getFilterId(), result.getResultDesc());
                    subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
                } else {
                    subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                    subscribe.setFilterId(result.getData());
                }
                subscribeService.updateById(subscribe);

            } catch (Exception e) {
                log.error("[Dao4ArtSchedule] subscripe retry error filterId:{} status e{}", subscribe.getFilterId(), e);
            }
        }
    }


    /**
     * 同步用户erc20余额
     */
    @Async
    @Scheduled(cron = "0/5 * * * * ? ")
    public void syncErc20Balance() {

        log.info("[Dao4ArtDexSchedule-syncErc20Balance] start");
        List<UserLiquidityStatistics> userLiquidityStatisticsList = userLiquidityStatisticsService.needSyncErc20Balance();
        if (userLiquidityStatisticsList.size() > 0) {
            for (UserLiquidityStatistics userLiquidityStatistics : userLiquidityStatisticsList) {
                try {
                    //查询pair合约toAddress的余额
                    InfuraCallRequestDto indexRequest = new InfuraCallRequestDto();
                    indexRequest.setNetWork(Dao4ArtDexConstant.netWork);
                    indexRequest.setTo(userLiquidityStatistics.getErc20Address());
                    indexRequest.setData(ContractMethodEnum.BALANCE_OF.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(userLiquidityStatistics.getUserAddress())));

                    Result<String> indexResult = subscriptionService.infuraCall(indexRequest);
                    if (indexResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                        log.error("[Dao4ArtDexSchedule-syncErc20Balance] userLiquidityStatistics:{} 查询余额异常 result:{}", JacksonUtil.obj2json(userLiquidityStatistics), indexResult.getResultDesc());
                    } else {
                        String indexResultData = indexResult.getData();
                        String pairAmount = CommonUtil.hexToTenString(indexResultData);
                        if (StringUtils.isBlank(pairAmount)) {
                            log.error("[Dao4ArtDexSchedule-syncErc20Balance] userLiquidityStatistics:{} pairAmount is null result:{}", JacksonUtil.obj2json(userLiquidityStatistics), indexResult.getResultDesc());
                            continue;
                        }

                        // 如果dao是三方20，token除以decimal，，，，
                        // 此处无法判断是否是三方20，或者需要再次调用infura查询，，所以直接查询decimal，然后进行计算
                        indexRequest.setData(ContractMethodEnum.DECIMALS.getMethodAddress());
                        Result<String> decimalResult = subscriptionService.infuraCall(indexRequest);
                        log.info("[erc20Decimals]erc20Address:{} resultData:{}", userLiquidityStatistics.getErc20Address(), decimalResult.getData());
                        if (decimalResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                            log.error("[erc20Decimals]erc20Address:{} error result:{}", userLiquidityStatistics.getErc20Address(), decimalResult.getResultDesc());
                        } else {
                            List<String> dataList = CommonUtil.splitBy32Bytes(decimalResult.getData());
                            String decimal = CommonUtil.hexToTenString(dataList.get(0));

                            // BigDecimal pairAmountDecimal = new BigDecimal(pairAmount).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                            BigDecimal pairAmountDecimal = new BigDecimal(pairAmount).divide(new BigDecimal("10").pow(Integer.parseInt(decimal)));

                            log.info("[Dao4ArtDexSchedule-syncErc20Balance] address:{} Erc20Balance:{} changeTo:{}", userLiquidityStatistics.getUserAddress(), userLiquidityStatistics.getErc20Balance(), pairAmountDecimal);
                            userLiquidityStatistics.setErc20Balance(pairAmountDecimal);
                            userLiquidityStatistics.setSyncErc20Balance(OneOrZeroEnum.ZERO.getStatus());
                            userLiquidityStatisticsService.updateById(userLiquidityStatistics);
                        }

                    }
                } catch (Exception e) {
                    log.error("[Dao4ArtDexSchedule-syncErc20Balance] userLiquidityStatistics :{} Exception e", JacksonUtil.obj2json(userLiquidityStatistics), e);
                }
            }
        }
    }


    /**
     * 每小时统计上一个小时的swap数据
     */
    @Async
    @Scheduled(cron = "1 0 0/1 * * ? ")
//    @Scheduled(cron = "0 0/5 * * * ? ")
    public void statisticsLastHourSwapVolume() {
        log.info("[Dao4ArtDexSchedule-statisticsLastHourSwapVolume] start");
        //先查询所有的erc20Address 然后遍历查询每个erc20的交易量
        long lastHour = DateUtil.getLastHourBeginTime().getTime() / 1000;
        long currentHour = DateUtil.getBeginOfCurrentHour().getTime() / 1000;

        List<Erc20Liquidity> erc20LiquidityList = erc20LiquidityService.selectErc20LiquidityStarted();
        if (erc20LiquidityList.size() == 0) {
            return;
        }
        for (Erc20Liquidity erc20Liquidity : erc20LiquidityList) {
            BigDecimal volume = liquidityTransactionService.getSwapVolume(erc20Liquidity.getErc20Address(), lastHour, currentHour);
            if (volume != null && volume.compareTo(BigDecimal.ZERO) > 0) {
                LiquidityPriceRecord lastHourLiquidityPriceRecord = liquidityPriceRecordService.getLastHoursPriceRecord(erc20Liquidity.getErc20Address(), OneOrZeroEnum.ZERO.getStatus(), lastHour);
                if (lastHourLiquidityPriceRecord != null) {
                    log.info("[Dao4ArtDexSchedule-statisticsLastHourSwapVolume] lastHourLiquidityPriceRecord is exist erc20Address：{}", erc20Liquidity.getErc20Address());
                    continue;
                }
                LiquidityPriceRecord liquidityPriceRecord = new LiquidityPriceRecord();
                // 类型 0-swap交易量 1-burn交易量
                liquidityPriceRecord.setType(OneOrZeroEnum.ZERO.getStatus());
                liquidityPriceRecord.setErc20Address(erc20Liquidity.getErc20Address());

                liquidityPriceRecord.setErc20Amount(erc20Liquidity.getErc20Balance());
                liquidityPriceRecord.setEthAmount(erc20Liquidity.getEthBalance());
                liquidityPriceRecord.setTradingVolume(volume);
                liquidityPriceRecord.setPrice(erc20Liquidity.getEthBalance().divide(erc20Liquidity.getErc20Balance(), 18, RoundingMode.FLOOR));
                liquidityPriceRecord.setRecordTime(lastHour);
                log.info("[Dao4ArtDexSchedule-statisticsLastHourSwapVolume] add liquidityPriceRecord:{}", JacksonUtil.obj2json(liquidityPriceRecord));
                liquidityPriceRecordService.save(liquidityPriceRecord);
            } else {
                LiquidityPriceRecord liquidityPriceRecord = liquidityPriceRecordService.getLatestSwapPrice(erc20Liquidity.getErc20Address());
                if (liquidityPriceRecord == null && erc20Liquidity.getErc20Balance() != null && erc20Liquidity.getEthBalance() != null && erc20Liquidity.getErc20Balance().compareTo(BigDecimal.ZERO) > 0 && erc20Liquidity.getEthBalance().compareTo(BigDecimal.ZERO) > 0) {
                    liquidityPriceRecord = new LiquidityPriceRecord();
                    // 类型 0-swap交易量 1-burn交易量
                    liquidityPriceRecord.setType(OneOrZeroEnum.ZERO.getStatus());
                    liquidityPriceRecord.setErc20Address(erc20Liquidity.getErc20Address());

                    liquidityPriceRecord.setErc20Amount(erc20Liquidity.getErc20Balance());
                    liquidityPriceRecord.setEthAmount(erc20Liquidity.getEthBalance());
                    liquidityPriceRecord.setTradingVolume(BigDecimal.ZERO);
                    liquidityPriceRecord.setPrice(erc20Liquidity.getEthBalance().divide(erc20Liquidity.getErc20Balance(), 18, RoundingMode.FLOOR));
                    liquidityPriceRecord.setRecordTime(lastHour);
                    log.info("[Dao4ArtDexSchedule-statisticsLastHourSwapVolume] first add liquidityPriceRecord:{}", JacksonUtil.obj2json(liquidityPriceRecord));
                    liquidityPriceRecordService.save(liquidityPriceRecord);
                }
            }
        }
        log.info("[Dao4ArtDexSchedule-statisticsLastHourSwapVolume] end");
    }

    /**
     * 每小时统计上一个小时的burn数据
     * 如果没有burn交易查询交易价格，价格变化了记录数据
     */
    @Async
    @Scheduled(cron = "1 0 0/1 * * ? ")
    public void statisticsLastHourBurnVolume() {
        log.info("[Dao4ArtDexSchedule-statisticsLastHourBurnVolume] start");
        //先查询所有的erc20Address 然后遍历查询每个erc20的交易量
        long lastHour = DateUtil.getLastHourBeginTime().getTime() / 1000;
        long currentHour = DateUtil.getBeginOfCurrentHour().getTime() / 1000;

        List<Erc20Liquidity> erc20LiquidityList = erc20LiquidityService.selectAllErc20Liquidity();
        log.info("[Dao4ArtDexSchedule-statisticsLastHourBurnVolume] size:{}", erc20LiquidityList.size());
        if (erc20LiquidityList.size() == 0) {
            return;
        }
        for (Erc20Liquidity erc20Liquidity : erc20LiquidityList) {
            BigDecimal volume = liquidityTransactionService.getBurnVolume(erc20Liquidity.getErc20Address(), lastHour, currentHour);
            if (volume != null && volume.compareTo(BigDecimal.ZERO) > 0) {
                LiquidityPriceRecord lastHourLiquidityPriceRecord = liquidityPriceRecordService.getLastHoursPriceRecord(erc20Liquidity.getErc20Address(), OneOrZeroEnum.ONE.getStatus(), lastHour);
                if (lastHourLiquidityPriceRecord != null) {
                    log.info("statisticsLastHourBurnVolume lastHourLiquidityPriceRecord is exist erc20Address：{}", erc20Liquidity.getErc20Address());
                    continue;
                }
                LiquidityPriceRecord liquidityPriceRecord = new LiquidityPriceRecord();
                // 类型 0-swap交易量 1-burn交易量
                liquidityPriceRecord.setType(OneOrZeroEnum.ONE.getStatus());
                liquidityPriceRecord.setErc20Address(erc20Liquidity.getErc20Address());
                liquidityPriceRecord.setTradingVolume(volume);

//                BigDecimal price = getTokenToETH(erc20Liquidity.getProjectId(), BigDecimal.ONE);
//                liquidityPriceRecord.setPrice(price);
                UserLiquidityStatistics userLiquidityStatistics = userLiquidityStatisticsService.selectHaveErc20UserAddress(erc20Liquidity.getErc20Address());
                if (userLiquidityStatistics != null) {
                    BigDecimal price = transferEthForContract(erc20Liquidity.getProjectId(), BigDecimal.ONE, userLiquidityStatistics.getUserAddress(), erc20Liquidity.getDaoVersion());
                    liquidityPriceRecord.setPrice(price);
                } else {
                    log.warn("statisticsLastHourBurnVolume-获取userLiquidityStatistics为空-Erc20Address：{}", erc20Liquidity.getErc20Address());
                    BigDecimal price = transferEthForContract(erc20Liquidity.getProjectId(), BigDecimal.ONE, erc20Liquidity.getCreateAddress(), erc20Liquidity.getDaoVersion());
                    liquidityPriceRecord.setPrice(price);
                }

                liquidityPriceRecord.setRecordTime(lastHour);
                log.info("[Dao4ArtDexSchedule-statisticsLastHourBurnVolume] add liquidityPriceRecord:{}", JacksonUtil.obj2json(liquidityPriceRecord));
                liquidityPriceRecordService.save(liquidityPriceRecord);
            } else {
                //查询当前价格 2.查询上一条记录 两个价格对比，如果不同则记录
//                BigDecimal price = getTokenToETH(erc20Liquidity.getProjectId(), BigDecimal.ONE);
                BigDecimal price;
                UserLiquidityStatistics userLiquidityStatistics = userLiquidityStatisticsService.selectHaveErc20UserAddress(erc20Liquidity.getErc20Address());
                if (userLiquidityStatistics != null) {
                    price = transferEthForContract(erc20Liquidity.getProjectId(), BigDecimal.ONE, userLiquidityStatistics.getUserAddress(), erc20Liquidity.getDaoVersion());
                } else {
                    log.warn("statisticsLastHourBurnVolume-获取userLiquidityStatistics为空-Erc20Address：{}", erc20Liquidity.getErc20Address());
                    price = transferEthForContract(erc20Liquidity.getProjectId(), BigDecimal.ONE, erc20Liquidity.getCreateAddress(), erc20Liquidity.getDaoVersion());
                }
                // 类型 0-swap交易量 1-burn交易量
                LiquidityPriceRecord liquidityPriceRecord = liquidityPriceRecordService.getLatestAssetPoolPrice(erc20Liquidity.getErc20Address());
                if (liquidityPriceRecord == null || !liquidityPriceRecord.getPrice().equals(price)) {
                    if (liquidityPriceRecord != null) {
                        log.info("[statisticsLastHourBurnVolume]erc20Address:{}-lastBrunPrice:{} newBurnPrice:{}", erc20Liquidity.getErc20Address(), liquidityPriceRecord.getPrice(), price);
                    }
                    LiquidityPriceRecord liquidityPriceRecord1 = new LiquidityPriceRecord();
                    liquidityPriceRecord1.setType(OneOrZeroEnum.ONE.getStatus());
                    liquidityPriceRecord1.setErc20Address(erc20Liquidity.getErc20Address());
                    liquidityPriceRecord1.setTradingVolume(BigDecimal.ZERO);
                    liquidityPriceRecord1.setPrice(price);
                    liquidityPriceRecord1.setRecordTime(lastHour);
                    log.info("[Dao4ArtDexSchedule-statisticsLastHourBurnVolume] first add liquidityPriceRecord:{}", JacksonUtil.obj2json(liquidityPriceRecord));
                    liquidityPriceRecordService.save(liquidityPriceRecord1);
                }
            }
        }
        log.info("[Dao4ArtDexSchedule-statisticsLastHourBurnVolume] end");
    }


    /**
     * 每天计算APY的值
     */
    @Async
    @Scheduled(cron = "1 0 0,9,12 * * ? ")
    public void statisticsLastDayApy() {
        log.info("[Dao4ArtDexSchedule-statisticsLastDayApy] start");
        List<Erc20Liquidity> erc20LiquidityList = erc20LiquidityService.selectAllErc20Liquidity();
        log.info("[Dao4ArtDexSchedule-statisticsLastDayApy] size:{}", erc20LiquidityList.size());
        if (erc20LiquidityList.size() == 0) {
            return;
        }
        Timestamp today = DateUtil.getBeginOfToday();
        long beforeYesterdayBeginHour = DateUtil.getTimestampAfterDay(today, -2).getTime() / 1000;
        long lastDayBeginHour = DateUtil.getTimestampAfterDay(today, -1).getTime() / 1000;
        long endHour = today.getTime() / 1000;
        for (Erc20Liquidity erc20Liquidity : erc20LiquidityList) {
            LiquidityDailyStatistics lastLiquidityDailyStatistics = liquidityDailyStatisticsService.getErc20DailyStatisticsLastDay(erc20Liquidity.getErc20Address(), lastDayBeginHour);
            if (lastLiquidityDailyStatistics != null && lastLiquidityDailyStatistics.getRecordTime().equals(lastDayBeginHour)) {
                log.info("statisticsLastDayApy-lastLiquidityDailyStatistics is exist erc20Address：{}", erc20Liquidity.getErc20Address());
                continue;
            }
            ResultList<DaoErc20Dto> daoErc20Result;
            if (erc20Liquidity.getAppSource() == 1) {
                daoErc20Result = dao4ArtService.getDaoErcInfo(Collections.singletonList(erc20Liquidity.getErc20Address()));
            } else {
                daoErc20Result = protoDaoService.getDaoErcInfo(Collections.singletonList(erc20Liquidity.getErc20Address()));
            }
            log.info("statisticsLastDayApy-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
            if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
                log.error("statisticsLastDayApy-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
                continue;
            }
            DaoErc20Dto daoErc20Dto = daoErc20Result.getDataList().get(0);
            Result<String> result = subscriptionService.ethGetBalance(Dao4ArtDexConstant.netWork, daoErc20Dto.getDaoAssetPool());
            if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[statisticsLastDayApy]infura ethGetBalance return data:{}", result.getData());
                String balance = result.getData();
                String price = CommonUtil.hexToTenString(balance);
                if (StringUtils.isNotBlank(price)) {
                    BigDecimal priceDecimal = new BigDecimal(price).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
                    LiquidityDailyStatistics beforeYesterdayLiquidityDailyStatistics = liquidityDailyStatisticsService.getErc20DailyStatisticsLastDay(erc20Liquidity.getErc20Address(), beforeYesterdayBeginHour);
                    BigDecimal lastBurnPrice = beforeYesterdayLiquidityDailyStatistics == null ? BigDecimal.ZERO : beforeYesterdayLiquidityDailyStatistics.getBurnPrice();
                    BigDecimal lastAssetPool = beforeYesterdayLiquidityDailyStatistics == null ? BigDecimal.ZERO : beforeYesterdayLiquidityDailyStatistics.getAssetPoolTotalAmount();

                    LiquidityDailyStatistics liquidityDailyStatistics = new LiquidityDailyStatistics();
                    liquidityDailyStatistics.setErc20Address(erc20Liquidity.getErc20Address());
                    liquidityDailyStatistics.setErc20Amount(erc20Liquidity.getErc20Balance());
                    liquidityDailyStatistics.setEthAmount(erc20Liquidity.getEthBalance());
                    if (erc20Liquidity.getErc20Balance() != null && erc20Liquidity.getEthBalance() != null) {
                        liquidityDailyStatistics.setEthSwappedRate(erc20Liquidity.getErc20Balance().divide(erc20Liquidity.getEthBalance(), 18, RoundingMode.FLOOR));
                        liquidityDailyStatistics.setErc20SwappedRate(erc20Liquidity.getEthBalance().divide(erc20Liquidity.getErc20Balance(), 18, RoundingMode.FLOOR));
                    }
//                    if (priceDecimal.compareTo(BigDecimal.ZERO) > 0) {
//                        liquidityDailyStatistics.setApy(priceDecimal.subtract(lastEth).divide(priceDecimal, 18, RoundingMode.FLOOR));
//                    } else {
//                        liquidityDailyStatistics.setApy(BigDecimal.ZERO);
//                    }
                    //计算APY（第二天价格-第一天价格）➗第一天价格）
                    BigDecimal burnPrice = BigDecimal.ZERO;
                    if (daoErc20Dto.getValuableSupply() != null && daoErc20Dto.getValuableSupply().compareTo(BigDecimal.ZERO) > 0) {
                        log.info("[statisticsLastDayApy]erc20Address:{} priceDecimal:{} ValuableSupply:{} lastBurnPrice:{}", erc20Liquidity.getErc20Address(), priceDecimal, daoErc20Dto.getValuableSupply(), lastBurnPrice);
                        burnPrice = priceDecimal.divide(daoErc20Dto.getValuableSupply(), 18, RoundingMode.FLOOR);
                    }
                    liquidityDailyStatistics.setBurnPrice(burnPrice);
                    if (lastBurnPrice.compareTo(BigDecimal.ZERO) > 0) {
                        liquidityDailyStatistics.setApy(burnPrice.subtract(lastBurnPrice).divide(lastBurnPrice, 18, RoundingMode.FLOOR));
                    } else {
                        liquidityDailyStatistics.setApy(BigDecimal.ZERO);
                    }

                    //计算feePool资金池变化
                    BigDecimal variaTion = priceDecimal.subtract(lastAssetPool);
                    //支出只有burn交易的数量 查询昨天一天burn交易的eth数量
                    BigDecimal costVolume = liquidityTransactionService.getBurnVolume(erc20Liquidity.getErc20Address(), lastDayBeginHour, endHour);
                    costVolume = costVolume == null ? BigDecimal.ZERO : costVolume;
                    liquidityDailyStatistics.setAssetPoolTotalAmount(priceDecimal);
                    //收入 = 资金池变化量 + 支出
                    liquidityDailyStatistics.setAssetPoolIncome(variaTion.add(costVolume));
                    liquidityDailyStatistics.setAssetPoolCost(costVolume);
                    liquidityDailyStatistics.setAssetPoolVariation(variaTion);

                    liquidityDailyStatistics.setRecordTime(lastDayBeginHour);
                    log.info("[Dao4ArtDexSchedule-statisticsLastDayApy] add liquidityDailyStatistics:{}", JacksonUtil.obj2json(liquidityDailyStatistics));
                    liquidityDailyStatisticsService.save(liquidityDailyStatistics);

                }
            } else {
                log.error("[currentDrbDaoStatistics]infura ethGetBalance return data:{} daoId:{}", result.getData(), erc20Liquidity.getDaoId());
            }


        }
        log.info("[Dao4ArtDexSchedule-statisticsLastDayApy] end");

    }

    @Async
    @Scheduled(cron = "0 0/5 * * * ? ")
    public void freshUserIsContract() {// 每五分钟执行一次
        log.info("[freshUserIsContract] running...");

        List<UserLiquidityStatistics> userList = userLiquidityStatisticsService.needSyncIsContract();
        log.info("[freshUserIsContract] size:{}", userList.size());
        if (userList.size() > 0) {
            for (UserLiquidityStatistics user : userList) {
                Result<Boolean> resultBoolean = subscriptionService.ethGetCodeCheckUserAddress(Dao4ArtDexConstant.netWork, user.getUserAddress());
                if (resultBoolean != null && resultBoolean.getData()) {
                    user.setIsContract(0);
                } else {
                    user.setIsContract(1);
                }
                userLiquidityStatisticsService.updateById(user);
            }
        }
        log.info("[freshUserIsContract] ending...");

    }


    //===================================private method separate==============================================================================//

    /**
     * function exchangeERC20ToETH(bytes32 _project_id, uint256 amount, address _to) public nonReentrant
     * returns(uint256){
     *
     * @param projectId   projectId
     * @param tokenAmount tokenAmount
     * @param userAddress userAddress
     * @return BigDecimal
     */
    private BigDecimal transferEthForContract(String projectId, BigDecimal tokenAmount, String userAddress, Integer daoVersion) {

        log.info("[transferEthForContract] projectId:{} tokenAmount:{} userAddress:{}", projectId, tokenAmount, userAddress);
        try {
            if (StringUtils.isAnyBlank(projectId, userAddress) || tokenAmount == null) {
                return BigDecimal.ZERO;
            }
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(Dao4ArtDexConstant.netWork);
//            if (daoVersion == null || daoVersion < 3) {
//                infuraCallRequestDto.setTo(Dao4ArtDexConstant.protocolContract);
//            } else {
//                infuraCallRequestDto.setTo(Dao4ArtDexConstant.protocolContractV2);
//            }
            // Dao4ArtDexConstant.protocolContractV2 合约已不用,注释掉
            infuraCallRequestDto.setTo(Dao4ArtDexConstant.protocolContract);


            infuraCallRequestDto.setFrom(userAddress);
            String token = decToHex(tokenAmount.multiply(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO)).stripTrailingZeros().toPlainString());
            String data = Dao4ArtDexConstant.exchangeERC20ToETH + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(projectId)) + CommonUtil.fillLeadingZerosInBytes32(token) + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(userAddress));

            log.info("[transferEthForContract] data:{}", data);
            infuraCallRequestDto.setData(data);

            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                if (StringUtils.isNotBlank(result.getResultDesc()) && result.getResultDesc().contains("burn amount exceeds balance")) {
                    log.warn("[transferEthForContract] error result:{} ", result.getResultDesc());
                    return BigDecimal.ZERO;
                } else {
                    log.error("[transferEthForContract] error result:{} ", result.getResultDesc());
                }
                throw new RuntimeException(result.getResultDesc());
            }
            log.info("[transferEthForContract]infura return data:{}", result.getData());
            String canvasInfoData = result.getData();
            String price = CommonUtil.hexToTenString(canvasInfoData);

            return new BigDecimal(price).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
        } catch (Exception e) {
            log.error("[transferEthForContract] projectId:{} tokenAmount:{} userAddress:{} e", projectId, tokenAmount, userAddress, e);
        }
        return BigDecimal.ZERO;

    }

    /**
     * function getTokenToETH(bytes32 projectId, uint256 tokenAmount) public view returns (uint256 ethAmount)
     *
     * @param projectId   projectId
     * @param tokenAmount tokenAmount
     * @return BigDecimal
     */
    @Deprecated
    private BigDecimal getTokenToETH(String projectId, BigDecimal tokenAmount) {

        log.info("[getTokenToETH] projectId:{} tokenAmount:{}", projectId, tokenAmount);
        try {
            if (StringUtils.isBlank(projectId)) {
                return BigDecimal.ZERO;
            }
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(Dao4ArtDexConstant.netWork);
            infuraCallRequestDto.setTo(Dao4ArtDexConstant.protocolContract);

            String token = decToHex(tokenAmount.multiply(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO)).stripTrailingZeros().toPlainString());
            String data = Dao4ArtDexConstant.getTokenToETH + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(projectId)) + CommonUtil.fillLeadingZerosInBytes32(token);

            log.info("[getTokenToETH] data:{}", data);
            infuraCallRequestDto.setData(data);

            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                if (StringUtils.isNotBlank(result.getResultDesc()) && result.getResultDesc().contains("burn amount exceeds balance")) {
                    log.warn("[getTokenToETH] error result:{} ", result.getResultDesc());
                    return BigDecimal.ZERO;
                } else {
                    log.error("[getTokenToETH] error result:{} ", result.getResultDesc());
                }
                throw new RuntimeException(result.getResultDesc());
            }
            log.info("[getTokenToETH]infura return data:{}", result.getData());
            String canvasInfoData = result.getData();
            String price = CommonUtil.hexToTenString(canvasInfoData);

            return new BigDecimal(price).divide(new BigDecimal(Dao4ArtDexConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);
        } catch (Exception e) {
            log.error("[getTokenToETH] projectId:{} tokenAmount:{} e:", projectId, tokenAmount, e);
        }
        return BigDecimal.ZERO;

    }

    /**
     * 十进制数据转换为十六进制字符串数
     *
     * @param dec
     * @return
     */
    private String decToHex(String dec) {
        BigDecimal sixteen = new BigDecimal(16);
        BigDecimal n = new BigDecimal(dec);
        StringBuffer sb = new StringBuffer(8);
        String a;
        char[] b = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
        /** 判断是否为0 */
        if (n.compareTo(BigDecimal.ZERO) == 0) {
            return n.toString();
        }
        while (n.compareTo(BigDecimal.ZERO) != 0) {
            /** BigDecimal除法取余数 */
            sb = sb.append(b[n.remainder(sixteen).intValue()]);
            /** BigDecimal除法,省略小数点后的位数且不进位 */
            n = n.divide(sixteen, 0, BigDecimal.ROUND_DOWN);
        }
        a = sb.reverse().toString();
        return a;
    }
}
