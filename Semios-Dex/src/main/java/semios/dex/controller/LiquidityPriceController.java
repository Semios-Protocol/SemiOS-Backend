package semios.dex.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.common.*;
import semios.dex.model.dto.feign.DaoErc20Dto;
import semios.dex.model.dto.response.Erc20PriceResDto;
import semios.dex.model.dto.response.Erc20ResDto;
import semios.dex.model.dto.response.Erc20SwapResDto;
import semios.dex.model.dto.response.Erc20TradeResDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.LiquidityDailyStatistics;
import semios.dex.model.entity.LiquidityPriceRecord;
import semios.dex.model.vo.PageVo;
import semios.dex.model.vo.req.Erc20QueryReqVo;
import semios.dex.model.vo.req.Erc20ReqVo;
import semios.dex.model.vo.res.ApyFoldLineResVo;
import semios.dex.model.vo.res.LiquidityStatisticsResVo;
import semios.dex.model.vo.res.PriceFoldLineResVo;
import semios.dex.model.vo.res.TrendingResVo;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.service.ILiquidityDailyStatisticsService;
import semios.dex.service.ILiquidityPriceRecordService;
import semios.dex.service.ILiquidityTransactionService;
import semios.dex.service.feign.IDao4ArtService;
import semios.dex.service.feign.IProtoDaoService;
import semios.dex.utils.DateUtil;
import semios.dex.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * dex 页面Price内容展示
 *
 * @author xiangbin
 */
@Slf4j
@RestController
@RequestMapping("/liquidity/price")
public class LiquidityPriceController extends BaseController {

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    @Autowired
    private ILiquidityPriceRecordService liquidityPriceRecordService;

    @Autowired
    private ILiquidityDailyStatisticsService liquidityDailyStatisticsService;

    @Autowired
    private IDao4ArtService dao4ArtService;

    @Autowired
    private IProtoDaoService protoDaoService;

    /**
     * erc20的price图表数据查询接口
     *
     * @return
     * @apiNote erc20的price图表数据查询
     */
    @PostMapping("/price_fold_line")
    public Result<PriceFoldLineResVo> priceOnLiquidity(@RequestBody(required = true) Erc20QueryReqVo erc20QueryReqVo) {
        Result<PriceFoldLineResVo> result = new Result<PriceFoldLineResVo>();
        if (!checkAddress(erc20QueryReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20QueryReqVo.getErc20Address());

        Erc20ResDto erc20ResDto = erc20LiquidityService.getErc20Price(erc20Address);

        List<Long> time = new ArrayList<Long>();
        List<String> volume = new ArrayList<String>();
        List<String> price = new ArrayList<String>();

        if (erc20ResDto == null) {
            PriceFoldLineResVo priceFoldLineResVo = new PriceFoldLineResVo();
            priceFoldLineResVo.setTime(time);
            priceFoldLineResVo.setVolume(volume);
            priceFoldLineResVo.setPrice(price);
            result.setData(priceFoldLineResVo);
            return result;
        }

        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, (erc20QueryReqVo.getDayTime() - 1) * -1);
        long endDate = DateUtil.getBeginOfNextHour().getTime() / 1000;
        List<Erc20PriceResDto> priceRecordList =
                liquidityPriceRecordService.getHoursErc20Price(erc20Address, startDate, endDate);
        String latestPrice = erc20ResDto.getPrice();

        long currentHour = DateUtil.getBeginOfCurrentHour().getTime() / 1000;
        BigDecimal latestTradingVolume = liquidityTransactionService.getSwapVolume(erc20Address, currentHour, endDate);

        BigDecimal maxVolume = latestTradingVolume == null ? BigDecimal.ZERO : latestTradingVolume;
        BigDecimal maxPrice = new BigDecimal(latestPrice);

        if (priceRecordList.size() > 0) {
            Map<Long, Erc20PriceResDto> priceMap =
                    priceRecordList.stream().collect(Collectors.toMap(Erc20PriceResDto::getRecordTime, e -> e));

            Erc20PriceResDto erc20PriceResDto = priceRecordList.get(0);
            BigDecimal currentPrice = erc20PriceResDto.getPrice();

            if (erc20PriceResDto.getRecordTime() <= startDate) {
                time = DateUtil.getHoursList(startDate, endDate);
            } else {
                time = DateUtil.getHoursList(erc20PriceResDto.getRecordTime(), endDate);
            }

            for (int i = 0; i < time.size(); i++) {
                if (i == time.size() - 1) {
                    volume.add(
                            latestTradingVolume == null ? "0" : latestTradingVolume.stripTrailingZeros().toPlainString());
                    price.add(latestPrice);
                } else {
                    Erc20PriceResDto priceRecord = priceMap.get(time.get(i));
                    if (priceRecord != null) {
                        BigDecimal tradingVolume = priceRecord.getTradingVolume();
                        BigDecimal recordPrice = priceRecord.getPrice();
                        volume.add(tradingVolume.stripTrailingZeros().toPlainString());
                        price.add(recordPrice.stripTrailingZeros().toPlainString());
                        if (tradingVolume.compareTo(maxVolume) > 0) {
                            maxVolume = tradingVolume;
                        }
                        if (recordPrice.compareTo(maxPrice) > 0) {
                            maxPrice = recordPrice;
                        }
                        currentPrice = recordPrice;
                    } else {
                        volume.add("0");
                        price.add(currentPrice.stripTrailingZeros().toPlainString());
                    }
                }
            }
        }

        PriceFoldLineResVo priceFoldLineResVo = new PriceFoldLineResVo();
        priceFoldLineResVo.setTime(time);
        priceFoldLineResVo.setVolume(volume);
        priceFoldLineResVo.setPrice(price);
        priceFoldLineResVo.setMaxVolume(maxVolume.stripTrailingZeros().toPlainString());
        priceFoldLineResVo.setMaxPrice(maxPrice.stripTrailingZeros().toPlainString());
        result.setData(priceFoldLineResVo);

        return result;
    }

    /**
     * erc20的apy图表数据查询接口
     *
     * @return
     * @apiNote erc20的apy图表数据查询
     */
    @PostMapping("/apy")
    public Result<ApyFoldLineResVo> liquidityApy(@RequestBody(required = true) Erc20QueryReqVo erc20QueryReqVo) {
        Result<ApyFoldLineResVo> result = new Result<ApyFoldLineResVo>();
        if (!checkAddress(erc20QueryReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20QueryReqVo.getErc20Address());
        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, (erc20QueryReqVo.getDayTime() - 1) * -1);
        long endDate = DateUtil.getBeginOfNextHour().getTime() / 1000;
        List<LiquidityDailyStatistics> dailyStatisticslist =
                liquidityDailyStatisticsService.getErc20DailyStatistics(erc20Address, startDate, endDate);

        long sevenDate = DateUtil.getIntervalTime(today, DexConstant.INTERVAL_DAY);
        long todayDate = today.getTime() / 1000;
        long yesterdayDate = DateUtil.getIntervalTime(today, -1);
        long tomorrowDate = DateUtil.getIntervalTime(today, 1);

        List<Long> time = new ArrayList<Long>();
        List<String> apy = new ArrayList<String>();

        List<Long> dottedTime = new ArrayList<Long>();
        List<String> dottedApy = new ArrayList<String>();

        BigDecimal maxApy = BigDecimal.ZERO;
        List<BigDecimal> sevenDayList = new ArrayList<BigDecimal>();
        BigDecimal yesterdayBurnPrice = BigDecimal.ZERO;

        for (LiquidityDailyStatistics dailyStatistics : dailyStatisticslist) {
            BigDecimal dailyApy = dailyStatistics.getApy();
            dailyApy = dailyApy.compareTo(BigDecimal.ZERO) > 0 ? dailyApy : BigDecimal.ZERO;
            BigDecimal percentApy =
                    dailyApy.divide(new BigDecimal("1"), DexConstant.PERCENT_DECIMAL, RoundingMode.HALF_UP)
                            .multiply(new BigDecimal("100"));
            if (dailyStatistics.getRecordTime() >= sevenDate) {
                sevenDayList.add(percentApy);
            }
            if (dailyStatistics.getRecordTime() == yesterdayDate) {
                yesterdayBurnPrice = dailyStatistics.getBurnPrice();
            }
            time.add(dailyStatistics.getRecordTime());
            apy.add(percentApy.stripTrailingZeros().toPlainString());
            if (dailyApy.compareTo(maxApy) > 0) {
                maxApy = dailyApy;
            }
        }

        //1.8.1 当天的预估价格：上小时结束时资金池内的ETH数量➗上小时结束时Valuable Supply（ERC20总量10亿➖已经Burn的数量）。
        //上小时结束时资金池内的ETH数量 = 上个小时burn的价格*上个小时结束时Circulating Supply（已发放的数量减Burn的数量）
        BigDecimal percentApyToday = BigDecimal.ZERO;
        if (yesterdayBurnPrice.compareTo(BigDecimal.ZERO) > 0) {
            BigDecimal circulatingSupply;
            LiquidityPriceRecord priceRecord = liquidityPriceRecordService.getLatestAssetPoolPrice(erc20Address);
            if (priceRecord != null) {
                Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20Address);
                ResultList<DaoErc20Dto> daoErc20Result;
                if (erc20Liquidity.getAppSource() == 1) {
                    daoErc20Result = dao4ArtService.getDaoErcInfo(Collections.singletonList(erc20Address));
                } else {
                    daoErc20Result = protoDaoService.getDaoErcInfo(Collections.singletonList(erc20Address));
                }
                log.info("liquidityApy-erc20Address:{}-返回结果daoErc20Result：{}", erc20Address, JacksonUtil.obj2json(daoErc20Result));
                if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
                    log.error("liquidityApy-erc20Address:{}-返回结果异常daoErc20Result：{}", erc20Address, JacksonUtil.obj2json(daoErc20Result));
                } else {
                    if (daoErc20Result.getDataList().size() > 0) {
                        DaoErc20Dto daoErc20Dto = daoErc20Result.getDataList().get(0);
                        circulatingSupply = daoErc20Dto.getCirculatingSupply();
                        BigDecimal todayPrice = priceRecord.getPrice().multiply(circulatingSupply).divide(daoErc20Dto.getValuableSupply(), 18, RoundingMode.FLOOR);
                        percentApyToday = todayPrice.subtract(yesterdayBurnPrice)
                                .divide(yesterdayBurnPrice, DexConstant.PERCENT_DECIMAL, RoundingMode.HALF_UP)
                                .multiply(new BigDecimal("100"));
                    }
                }
            }
        }
        percentApyToday = percentApyToday.compareTo(BigDecimal.ZERO) > 0 ? percentApyToday : BigDecimal.ZERO;
        sevenDayList.add(percentApyToday);

        BigDecimal percentApyTomorrow = BigDecimal.ZERO;
        if (sevenDayList.size() > 0) {
            percentApyTomorrow = Collections.max(sevenDayList);
        }

        time.add(todayDate);
        apy.add(percentApyToday.stripTrailingZeros().toPlainString());

        dottedTime.add(todayDate);
        dottedApy.add(percentApyToday.stripTrailingZeros().toPlainString());
        dottedTime.add(tomorrowDate);
        dottedApy.add(percentApyTomorrow.stripTrailingZeros().toPlainString());

        ApyFoldLineResVo apyFoldLineResVo = new ApyFoldLineResVo();
        apyFoldLineResVo.setTime(time);
        apyFoldLineResVo.setApy(apy);
        apyFoldLineResVo.setDottedTime(dottedTime);
        apyFoldLineResVo.setDottedApy(dottedApy);
        apyFoldLineResVo.setMaxApy(maxApy.stripTrailingZeros().toPlainString());
        result.setData(apyFoldLineResVo);

        return result;
    }

    /**
     * erc20的dex统计信息查询接口
     *
     * @return
     * @apiNote erc20的dex统计信息查询
     */
    @PostMapping("/statistics")
    public Result<LiquidityStatisticsResVo> liquidityStatistics(@RequestBody(required = true) Erc20ReqVo erc20ReqVo)
            throws Exception {
        Result<LiquidityStatisticsResVo> result = new Result<>();
        if (!checkErc20ReqVo(erc20ReqVo)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        String erc20Address = formatAddress(erc20ReqVo.getErc20Address());
        LiquidityStatisticsResVo liquidityStatisticsResVo = new LiquidityStatisticsResVo();
        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, DexConstant.INTERVAL_DAY);
        long endDate = DateUtil.getIntervalTime(today, 1);
        BigDecimal totalVolume = liquidityTransactionService.getSwapVolume(erc20Address, null, null);
        if (totalVolume != null) {
            BigDecimal sevenDaysVolume = liquidityTransactionService.getSwapVolume(erc20Address, startDate, endDate);
            liquidityStatisticsResVo.setSevenDaysVolume(decimal2String(sevenDaysVolume));
            liquidityStatisticsResVo.setTotalVolume(decimal2String(totalVolume));
        }

        // 从主站获取
        // BigDecimal burnVolume = liquidityTransactionService.getBurnVolume(erc20Address, null, null);
        // if (burnVolume != null) {
        // liquidityStatisticsResVo.setBurnVolume(decimal2String(burnVolume));
        // }

        List<String> erc20AddressList = new ArrayList<String>();
        erc20AddressList.add(erc20Address);

        ResultList<DaoErc20Dto> daoErc20Result;
        log.info("owners erc20Address:" + erc20Address);
        Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20Address);
        log.info("get erc20Address:{},erc20Liquidity:{}", erc20Address, JacksonUtil.obj2json(erc20Liquidity));
        if (erc20Liquidity != null) {
            if (erc20Liquidity.getAppSource() == 1) {
                log.info("Dao4Art接口调用-获取dao信息-请求参数：{}", erc20AddressList);
                daoErc20Result = dao4ArtService.getDaoErcInfo(erc20AddressList);
                log.info("Dao4Art接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
            } else {
                log.info("ProtoDao接口调用-获取dao信息-请求参数：{}", erc20AddressList);
                daoErc20Result = protoDaoService.getDaoErcInfo(erc20AddressList);
                log.info("ProtoDao接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
            }

            if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
                throw new Exception("error get dao info from dao4art");
            }
            if (daoErc20Result.getDataList().size() > 0) {
                DaoErc20Dto daoErc20Dto = daoErc20Result.getDataList().get(0);
                liquidityStatisticsResVo.setTotalSupply(decimal2String(daoErc20Dto.getTotalSupply()));
                liquidityStatisticsResVo.setBurnVolume(decimal2String(daoErc20Dto.getBurnVolume()));
                liquidityStatisticsResVo.setCirculatingSupply(decimal2String(daoErc20Dto.getCirculatingSupply()));
                liquidityStatisticsResVo.setValuableSupply(decimal2String(daoErc20Dto.getValuableSupply()));
            }
        }

        result.setData(liquidityStatisticsResVo);
        return result;

    }

    /**
     * erc20的趋势信息查询接口
     *
     * @return
     * @apiNote erc20的趋势信息查询（每页10条数据）（返回数据集合dataList）
     */
    @PostMapping("/trending")
    public ResultList<TrendingResVo> trendingRoyaltyToken(@RequestBody(required = true) PageVo pageVo)
            throws Exception {
        ResultList<TrendingResVo> result = new ResultList<>();
        Date nextHour = DateUtil.getBeginOfNextHour();
        long startDate = DateUtil.getIntervalTime(nextHour, -1);
        long endDate = nextHour.getTime() / 1000;
        IPage<Erc20SwapResDto> erc20SwapPage = erc20LiquidityService.getErc20SwapVolumeList(pageVo, startDate, endDate);
        List<Erc20SwapResDto> erc20Swaplist = erc20SwapPage.getRecords();

        List<TrendingResVo> trendingList = new ArrayList<TrendingResVo>();

        // 当前页全部资金池的erc20
        List<String> allErc20AddressList = new ArrayList<String>();
        // 开通了资金池的erc20
        List<String> erc20AddressList = new ArrayList<String>();
        for (Erc20SwapResDto erc20SwapResDto : erc20Swaplist) {
            String erc20Address = erc20SwapResDto.getErc20Address();
            allErc20AddressList.add(erc20Address);
            TrendingResVo trendingResVo = new TrendingResVo();
            trendingResVo.setErc20Address(erc20Address);
            trendingResVo.setErc20Symbol(erc20SwapResDto.getErc20Symbol());
            trendingResVo.setPrice(erc20SwapResDto.getPrice());
            if (!trendingResVo.getPrice().equals("0")) {
                erc20AddressList.add(erc20Address);
            }
            trendingResVo.setOneDayVolume(erc20SwapResDto.getTotalVolume());
            trendingList.add(trendingResVo);
        }

        // 24小时前同小时价格map
        Map<String, BigDecimal> oneDayErc20PriceHash = new HashMap<String, BigDecimal>();

        // 7天前23点价格map
        Map<String, BigDecimal> sevenDayErc20PriceHash = new HashMap<String, BigDecimal>();

        // 查询24小时和7天前价格信息
        if (erc20AddressList.size() != 0) {
            Date currentHour = DateUtil.getBeginOfCurrentHour();
            long oneDayTime = DateUtil.getIntervalTime(currentHour, -1);
            List<Erc20PriceResDto> oneDayErc20PriceList =
                    liquidityPriceRecordService.getOneHourErc20Price(erc20AddressList, oneDayTime);
            for (Erc20PriceResDto erc20PriceResDto : oneDayErc20PriceList) {
                BigDecimal price = erc20PriceResDto.getPrice();
                if (price.compareTo(BigDecimal.ZERO) > 0) {
                    oneDayErc20PriceHash.put(erc20PriceResDto.getErc20Address(), price);
                }
            }
            Date lastHour = DateUtil.getBeginOfLastHour();
            long sevenDayTime = DateUtil.getIntervalTime(lastHour, DexConstant.INTERVAL_DAY);
            List<Erc20PriceResDto> sevenDayErc20PriceList =
                    liquidityPriceRecordService.getOneHourErc20Price(erc20AddressList, sevenDayTime);
            for (Erc20PriceResDto erc20PriceResDto : sevenDayErc20PriceList) {
                BigDecimal price = erc20PriceResDto.getPrice();
                if (price.compareTo(BigDecimal.ZERO) > 0) {
                    sevenDayErc20PriceHash.put(erc20PriceResDto.getErc20Address(), price);
                }
            }
        }

        // 48小时到24小时交易量map
        Map<String, BigDecimal> oneDayErc20SwapHash = new HashMap<String, BigDecimal>();

        // 查询48小时到24小时交易量
        if (erc20AddressList.size() != 0) {
            endDate = startDate;
            startDate = DateUtil.getIntervalTime(nextHour, -2);
            List<Erc20TradeResDto> erc20SwapVolumeList =
                    liquidityTransactionService.getSwapVolumeList(erc20AddressList, startDate, endDate);
            for (Erc20TradeResDto erc20TradeResDto : erc20SwapVolumeList) {
                oneDayErc20SwapHash.put(erc20TradeResDto.getErc20Address(), erc20TradeResDto.getTradingVolume());
            }
        }

        // 查询当前页erc20 burn的量
        // if (allErc20AddressList.size() != 0) {
        // List<Erc20TradeResDto> erc20BurnVolumeList =
        // liquidityTransactionService.getBurnVolumeList(allErc20AddressList, null, null);
        // }

//        log.info("Dao4Art接口调用-获取dao信息-请求参数：{}", allErc20AddressList);
//        ResultList<DaoErc20Dto> daoErc20Result = dao4ArtService.getDaoErcInfo(allErc20AddressList);
//        log.info("Dao4Art接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
//        if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
//            throw new Exception("error get dao info from dao4art");
//        }
//
//        // dao info map
//        Map<String, DaoErc20Dto> daoErc20Hash =
//                daoErc20Result.getDataList().stream().collect(Collectors.toMap(DaoErc20Dto::getErc20Address, e -> e, (v1, v2) -> v1));

        for (TrendingResVo trendingResVo : trendingList) {
            BigDecimal price = new BigDecimal(trendingResVo.getPrice());
            BigDecimal volume = new BigDecimal(trendingResVo.getOneDayVolume());
            String erc20Address = trendingResVo.getErc20Address();
            if (oneDayErc20PriceHash.containsKey(erc20Address)) {
                BigDecimal oneDayPrice = oneDayErc20PriceHash.get(erc20Address);
                trendingResVo.setOneDayChanged(
                        price.subtract(oneDayPrice).divide(oneDayPrice, DexConstant.PERCENT_DECIMAL, RoundingMode.HALF_UP)
                                .multiply(new BigDecimal("100")).stripTrailingZeros().toPlainString());
            }
            if (sevenDayErc20PriceHash.containsKey(erc20Address)) {
                BigDecimal sevenDayPrice = sevenDayErc20PriceHash.get(erc20Address);
                trendingResVo.setSevenDayChanged(price.subtract(sevenDayPrice)
                        .divide(sevenDayPrice, DexConstant.PERCENT_DECIMAL, RoundingMode.HALF_UP)
                        .multiply(new BigDecimal("100")).stripTrailingZeros().toPlainString());
            }
            if (oneDayErc20SwapHash.containsKey(erc20Address)) {
                BigDecimal swapAmount = oneDayErc20SwapHash.get(erc20Address);
                trendingResVo.setOneDayVolumeChanged(
                        volume.subtract(swapAmount).divide(swapAmount, DexConstant.PERCENT_DECIMAL, RoundingMode.HALF_UP)
                                .multiply(new BigDecimal("100")).stripTrailingZeros().toPlainString());
            }
//            if (daoErc20Hash.containsKey(erc20Address)) {
//            DaoErc20Dto daoErc20Dto = daoErc20Hash.get(erc20Address);
            ResultList<DaoErc20Dto> daoErc20Result;
            Erc20Liquidity erc20Liquidity = erc20LiquidityService.selectErc20LiquidityByErc20Address(erc20Address);
            List<String> erc20AddressArray = new ArrayList<>();
            erc20AddressArray.add(erc20Address);
            if (erc20Liquidity.getAppSource() == 1) {
                log.info("[asset_pool_info]Dao4Art接口调用-获取dao信息-请求参数：{}", erc20AddressArray);
                daoErc20Result = dao4ArtService.getDaoErcInfo(erc20AddressArray);
                log.info("[asset_pool_info]Dao4Art接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));

            } else {
                log.info("[asset_pool_info]ProtoDao接口调用-获取dao信息-请求参数：{}", erc20AddressArray);
                daoErc20Result = protoDaoService.getDaoErcInfo(erc20AddressArray);
                log.info("[asset_pool_info]ProtoDao接口调用-获取dao信息-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
            }
            if (ResultDesc.SUCCESS.getResultCode() != daoErc20Result.getResultCode()) {
                log.error("[asset_pool_info]获取dao信息-失败-返回结果daoErc20Result：{}", JacksonUtil.obj2json(daoErc20Result));
                continue;
//                throw new Exception("error get dao info from dao4art");
            }
            if (daoErc20Result.getDataList() != null && daoErc20Result.getDataList().size() > 0) {
                DaoErc20Dto daoErc20Dto = daoErc20Result.getDataList().get(0);

                trendingResVo.setDaoName(daoErc20Dto.getDaoName());
                trendingResVo.setDaoId(daoErc20Dto.getDaoId());
                trendingResVo.setFeePool(daoErc20Dto.getFeePool());
                trendingResVo.setDaoLogoUrl(daoErc20Dto.getDaoLogoUrl());
                trendingResVo.setMarketCap(
                        price.multiply(daoErc20Dto.getCirculatingSupply()).stripTrailingZeros().toPlainString());
                trendingResVo.setRoyaltyFee(decimal2String(daoErc20Dto.getRoyaltyFeeIncome()));
            }
//            }
        }

        result.setDataList(trendingList);
        PageDto page = new PageDto();
        page.setPageNo(pageVo.getPageNo());
        page.setPageSize(pageVo.getPageSize());
        page.setCount(erc20SwapPage.getTotal());
        result.setPage(page);
        return result;

    }

}
