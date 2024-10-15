package semios.api.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.interceptor.WebConfigurer;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.model.vo.TreasuryTogetherDaoListVo;
import semios.api.model.vo.req.*;
import semios.api.model.vo.res.*;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.IProtoDaoDexService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.CookieUtil;
import semios.api.utils.DateUtil;
import semios.api.utils.JacksonUtil;

import javax.servlet.http.HttpServletRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description: DAO Analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:10
 **/
@Slf4j
@RestController
@RequestMapping("/dao/analytics")
public class DaoAnalyticsController {

    @Autowired
    private IWorkService workService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IUserService userService;

    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;

    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    @Autowired(required = false)
    private IProtoDaoDexService protoDaoDexService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    @Autowired
    private IDaoDailyStatisticsService daoDailyStatisticsService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IFavoritesService favoritesService;

    @Autowired
    private ITreasuryTransactionService treasuryTransactionService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Value("${user_profile_image}")
    private String headImage;

    private static final int FIVE = 5;
    private static final int SIX = 6;
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    private static final String PRE_LOG = "[DaoAnalyticsController]-";

    private static final List<String> COLOR_LIST =
            Arrays.asList("#2ECAA9,#42BC83,#6BC5B0,#2BC9BF,#7EB991,#53F1D5,#96E0BD,#40A5BB,#61D9BC,#ADFFC9".split(","));

    /**
     * Analytics模块 头部6个统计信息 1.7
     */
    @PostMapping(value = "/statistics")
    public Result<DaoAnalyticsResVo> daoStatistics(@RequestBody DaoAnalyticsReqVo daoAnalyticsReqDto,
                                                   HttpServletRequest request) {
        Result<DaoAnalyticsResVo> result = new Result<>();
        List<DaoAnalyticsResVo> daoAnalyticsResDtoList = new ArrayList<>();
        log.info(PRE_LOG + "daoStatistics param:{}", JacksonUtil.obj2json(daoAnalyticsReqDto));
        if (daoAnalyticsReqDto == null || daoAnalyticsReqDto.getDaoId() == null
                || daoAnalyticsReqDto.getDayTime() == null) {
            return result;
        }
        Dao dao = daoService.getById(daoAnalyticsReqDto.getDaoId());
        if (dao == null) {
            log.warn(PRE_LOG + "daoStatistics dao is null param:{}", JacksonUtil.obj2json(daoAnalyticsReqDto));
            return result;
        }
        // 时间7天和14天的查询条件
        DaoAnalyticsBo daoAnalyticsBo = buildFirstDaoAnalyticsBo(daoAnalyticsReqDto);
        log.info("[daoStatistics] daoAnalyticsBo:{}", JacksonUtil.obj2json(daoAnalyticsBo));
        Integer nftOwners = workService.selectNftOwners(daoAnalyticsBo.getDaoId() + "");
        Integer endDateOwners =
                workService.selectNftOwnersByEndDate(daoAnalyticsBo.getDaoId() + "", daoAnalyticsBo.getStartDate());
        // 查询最近7天
        List<Work> firstWorkList = workService.selecWorkForAnalytics(daoAnalyticsBo);
        // buildErc20Owners
        daoAnalyticsBo.setProjectId(dao.getProjectId());
        List<TokenReceivedRecord> firstTokenReceivedList =
                tokenReceivedRecordService.recordListForAnalytics(daoAnalyticsBo);

        // 查询 7-14天 判断dao是否满足14天
        // daoAnalyticsBo 更新查询日期
        LocalDate localDate = LocalDate.now();
        List<Work> secondWorkList = null;
        DaoDrbStatistics daoDrbStatistics = null;
        if (dao.getDaoStartDate() != null
                && !(dao.getDaoStartDate().plusDays(daoAnalyticsReqDto.getDayTime() * 2 - 1).isAfter(localDate))) {
            //  这里改为日期 不用DRB
            Integer firstDrb = Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) - daoAnalyticsReqDto.getDayTime() + 1;
            int secondNum = daoAnalyticsReqDto.getDayTime() * 2 - 1;
            daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumberForAnalytics(dao.getId(), firstDrb);
            if (daoDrbStatistics == null
                    || daoDrbStatistics.getDrbNumber() < Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) - secondNum) {
                daoDrbStatistics = null;
            }

            log.info("[daoStatistics] daoDrbStatistics CURRENT_ROUND:{} firstDrb:{}", ProtoDaoConstant.CURRENT_ROUND,
                    firstDrb);

            buildSecondDaoAnalyticsBo(daoAnalyticsReqDto, daoAnalyticsBo);
            log.info("[daoStatistics] buildSecondDaoAnalyticsBo daoAnalyticsBo:{}",
                    JacksonUtil.obj2json(daoAnalyticsBo));
            secondWorkList = workService.selecWorkForAnalytics(daoAnalyticsBo);
        }

        daoAnalyticsResDtoList.add(DaoAnalyticsResVo.buildTotalVol(firstWorkList, secondWorkList));
        daoAnalyticsResDtoList.add(DaoAnalyticsResVo.buildDaoFloorPrice(dao, daoDrbStatistics));
        daoAnalyticsResDtoList.add(DaoAnalyticsResVo.buildMintQuantity(firstWorkList, secondWorkList));
        daoAnalyticsResDtoList.add(DaoAnalyticsResVo.buildTopBid(firstWorkList, secondWorkList));
        daoAnalyticsResDtoList.add(DaoAnalyticsResVo.buildNftOwners(nftOwners, endDateOwners, secondWorkList, dao));

        List<TokenReceivedRecord> secondTokenReceivedList = null;
        if (dao.getDaoStartDate() != null
                && !(dao.getDaoStartDate().plusDays(daoAnalyticsReqDto.getDayTime() * 2 - 1).isAfter(localDate))) {
            // 构建查询参数
            buildSecondDaoAnalyticsBo(daoAnalyticsReqDto, daoAnalyticsBo);
            secondTokenReceivedList = tokenReceivedRecordService.recordListForAnalytics(daoAnalyticsBo);
        }
        UserLiquidityStatisticsVo userLiquidityStatisticsVo = new UserLiquidityStatisticsVo();
        userLiquidityStatisticsVo.setErc20Address(dao.getErc20Token());
        Result<Integer> resultAmount = protoDaoDexService.erc20Owner(userLiquidityStatisticsVo);
        int amount = 0;
        if (resultAmount.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
            amount = resultAmount.getData();
        } else {
            log.error("[daoStatistics] buildErc20Owners resultAmount desc:{}", resultAmount.getResultDesc());
        }
        daoAnalyticsResDtoList.add(DaoAnalyticsResVo.buildErc20Owners(amount, firstTokenReceivedList,
                secondTokenReceivedList, secondWorkList, dao));

        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
            daoAnalyticsResDtoList.forEach(v -> v.setErc20PaymentMode(true));
        }

        for (DaoAnalyticsResVo v : daoAnalyticsResDtoList) {
            v.setDaoSymbol(dao.getDaoSymbol());
            v.setPayCurrencyType(dao.getPayCurrencyType());
            v.setInputTokenAddress(dao.getInputTokenAddress());
            v.setDaoErc20Address(dao.getErc20Token());
        }

        result.setDataList(daoAnalyticsResDtoList);
        return result;
    }

    /**
     * Trading Volume and Average Price 1.7
     * <p>
     * 七天内的总销售金额和平均铸造价格
     */
    @PostMapping(value = "/volume/averagePrice")
    public Result<DaoFoldLineResVo> tradingVolumeAndAveragePrice(@RequestBody DaoAnalyticsReqVo daoAnalyticsReqDto,
                                                                 HttpServletRequest request) {
        Result<DaoFoldLineResVo> result = new Result<>();
        DaoFoldLineResVo daoFoldLineResVo = new DaoFoldLineResVo();
        result.setData(daoFoldLineResVo);
        if (daoAnalyticsReqDto == null || daoAnalyticsReqDto.getDaoId() == null
                || daoAnalyticsReqDto.getDayTime() == null) {
            return result;
        }
        // 查询dao，获取dao的开始时间
        Dao dao = daoService.getById(daoAnalyticsReqDto.getDaoId());
        if (dao == null || DaoStatusEnum.NOT_STARTED.getStatus().equals(dao.getDaoStatus())) {
            return result;
        }
        LocalDate daoStartDate = dao.getDaoStartDate();

        Integer dayTime = daoAnalyticsReqDto.getDayTime();
        LocalDateTime localDateTime = LocalDateTime.now();
        LocalDateTime lastTimeDateTime = localDateTime.minusDays(dayTime - 1);
        if (lastTimeDateTime.isBefore(daoStartDate.atStartOfDay())) {
            lastTimeDateTime = daoStartDate.atStartOfDay();
        }
        String endDate = localDateTime.format(FORMATTER);
        String startDate = lastTimeDateTime.format(FORMATTER);
        log.info("[tradingVolumeAndAveragePrice] endDate:{} startDate:{}", endDate, startDate);
        DaoAnalyticsBo daoAnalyticsBo = new DaoAnalyticsBo();
        daoAnalyticsBo.setDaoId(daoAnalyticsReqDto.getDaoId());
        daoAnalyticsBo.setStartDate(startDate);
        daoAnalyticsBo.setEndDate(endDate);
        daoAnalyticsBo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        List<Work> workList = workService.selecWorkForAnalytics(daoAnalyticsBo);
        List<Long> times = new ArrayList<>();
        List<BigDecimal> volume = new ArrayList<>();
        List<BigDecimal> price = new ArrayList<>();
        log.info("[tradingVolumeAndAveragePrice] workList:{} ", JacksonUtil.obj2json(workList));
        do {
            LocalDateTime localDateTime1 = lastTimeDateTime.withHour(0).withMinute(0).withSecond(0).withNano(0);
            if (workList.size() > 0) {
                times.add(localDateTime1.toEpochSecond(ZoneOffset.UTC));
                List<Work> workList1 = workList.stream()
                        .filter(v -> LocalDateTime.ofEpochSecond(Long.parseLong(v.getBlockTime()), 0, ZoneOffset.UTC)
                                .withHour(0).withMinute(0).withSecond(0).withNano(0).equals(localDateTime1))
                        .collect(Collectors.toList());
                log.info("[tradingVolumeAndAveragePrice] workList size :{} workList1 times:{} localDateTime1:{}",
                        workList1.size(),
                        LocalDateTime.ofEpochSecond(Long.parseLong(workList.get(0).getBlockTime()), 0, ZoneOffset.UTC)
                                .withHour(0).withMinute(0).withSecond(0).withNano(0),
                        localDateTime1);
                if (workList1.size() > 0) {
                    BigDecimal volumeValue =
                            workList1.stream().map(Work::getMintedPrice).reduce(BigDecimal.ZERO, BigDecimal::add);
                    volume.add(volumeValue);
                    price.add(volumeValue.divide(new BigDecimal(workList1.size()), 4, RoundingMode.HALF_UP));
                } else {
                    volume.add(BigDecimal.ZERO);
                    price.add(BigDecimal.ZERO);
                }
            }

            lastTimeDateTime = lastTimeDateTime.plusDays(1);
            log.info("[tradingVolumeAndAveragePrice] lastTimeDateTime:{} endDate:{}",
                    lastTimeDateTime.format(FORMATTER), endDate);
        } while (lastTimeDateTime.format(FORMATTER).compareTo(endDate) <= 0);

        daoFoldLineResVo.setTimes(times);
        daoFoldLineResVo.setVolume(volume);
        daoFoldLineResVo.setPrice(price);
        if (volume.size() > 0) {
            daoFoldLineResVo.setMaxVolume(volume.stream().max(Comparator.comparing(BigDecimal::floatValue)).get());
        }
        if (price.size() > 0) {
            daoFoldLineResVo.setMaxPrice(price.stream().max(Comparator.comparing(BigDecimal::floatValue)).get());
        }

        return result;
    }

    /**
     * Price range of NFTs 1.7
     */
    @PostMapping(value = "/priceRange")
    public Result<DaoRangeResVo> nftPriceRange(@RequestBody DaoPriceRangeReqVo daoPriceRangeReqVo,
                                               HttpServletRequest request) {
        Result<DaoRangeResVo> result = new Result<>();
        List<DaoRangeResVo> daoPriceRangeResVoList = new ArrayList<>();
        if (daoPriceRangeReqVo == null || daoPriceRangeReqVo.getDaoId() == null) {
            return result;
        }
        List<Work> workList = workService.selectWorksByDaoIdAndStatus(daoPriceRangeReqVo.getDaoId() + "",
                WorkStatusEnum.CASTED.getStatus());
        log.info("[nftPriceRange] workList size:{}", workList.size());
        if (daoPriceRangeReqVo.getFixedPrice() == null || daoPriceRangeReqVo.getFixedPrice() == 0) {
            workList = workList.stream().filter(v -> !WorkPriceTypeEnum.FIXED_PRICE.getType().equals(v.getPriceType()))
                    .collect(Collectors.toList());
            log.info("[nftPriceRange] workList fixed price not included size:{}", workList.size());
        }
        Optional<Work> maxPriceWork = workList.stream().max(Comparator.comparing(Work::getMintedPrice));
        if (maxPriceWork.isPresent()) {
            BigDecimal maxPrice = maxPriceWork.get().getMintedPrice();
            if (maxPrice.compareTo(BigDecimal.ZERO) == 0) {
                DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
                String range = "0-0";
                daoRangeResVo.setRange(range);
                daoRangeResVo.setRatio(new BigDecimal("100"));
                daoPriceRangeResVoList.add(daoRangeResVo);
                result.setDataList(daoPriceRangeResVoList);
                return result;
            }
            BigDecimal averagePrice = maxPrice.divide(new BigDecimal(String.valueOf(FIVE)), 4, RoundingMode.HALF_UP);
            BigDecimal lowPrice = BigDecimal.ZERO;
            BigDecimal workSize = new BigDecimal(workList.size());
            BigDecimal ratioSize = BigDecimal.ZERO;
            // 固定为5个范围
            for (int i = 0; i < FIVE; i++) {
                DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
                BigDecimal endPriceRange = i == 4 ? maxPrice : lowPrice.add(averagePrice);
                BigDecimal lowPriceRange = lowPrice;
                String range = lowPriceRange.stripTrailingZeros().toPlainString() + "-"
                        + endPriceRange.stripTrailingZeros().toPlainString();
                daoRangeResVo.setRange(range);
                List<Work> workListRange =
                        workList.stream().filter(v -> v.getMintedPrice().compareTo(lowPriceRange) >= 0
                                && v.getMintedPrice().compareTo(endPriceRange) <= 0).collect(Collectors.toList());
                BigDecimal ratio = new BigDecimal(workListRange.size()).divide(workSize, 4, RoundingMode.HALF_UP);
                if (i == 4) {
                    ratio = BigDecimal.ONE.subtract(ratioSize);
                }
                daoRangeResVo.setRatio(ratio.multiply(new BigDecimal("100")).stripTrailingZeros());

                lowPrice = lowPrice.add(averagePrice);
                ratioSize = ratioSize.add(ratio);
                daoPriceRangeResVoList.add(daoRangeResVo);
            }
        }

        result.setDataList(daoPriceRangeResVoList);

        return result;
    }

    /**
     * Mint 铸造时间以散点图的形式记录到表格中 1.7
     */
    @PostMapping(value = "/mintScatterPlot")
    public Result<DaoScatterPlotResVo> mintScatterPlot(@RequestBody DaoAnalyticsReqVo daoAnalyticsReqDto,
                                                       HttpServletRequest request) {
        Result<DaoScatterPlotResVo> result = new Result<>();
        DaoScatterPlotResVo daoScatterPlotResVo = new DaoScatterPlotResVo();
        if (daoAnalyticsReqDto == null || daoAnalyticsReqDto.getDaoId() == null
                || daoAnalyticsReqDto.getDayTime() == null) {
            return result;
        }
        Integer dayTime = daoAnalyticsReqDto.getDayTime();
        LocalDateTime localDateTime = LocalDateTime.now();
        LocalDateTime lastTimeDateTime = localDateTime.minusDays(dayTime - 1);
        String firstStartDate = localDateTime.format(FORMATTER);
        String secondStartDate = lastTimeDateTime.format(FORMATTER);
        Dao dao = daoService.getById(daoAnalyticsReqDto.getDaoId());
        if (dao == null || DaoStatusEnum.NOT_STARTED.getStatus().equals(dao.getDaoStatus())) {
            daoScatterPlotResVo.setXtime(new ArrayList<>());
            daoScatterPlotResVo.setPrice(new ArrayList<>());
            daoScatterPlotResVo.setName(new ArrayList<>());
            daoScatterPlotResVo.setImage(new ArrayList<>());
            result.setData(daoScatterPlotResVo);
            return result;
        }
        // LocalDate daoStartDate = dao.getDaoStartDate();

        // if (lastTimeDateTime.isBefore(daoStartDate.atStartOfDay())) {
        // lastTimeDateTime = daoStartDate.atStartOfDay();
        // }
        log.info("[mintScatterPlot] firstStartDate:{} secondStartDate:{}", firstStartDate, secondStartDate);
        DaoAnalyticsBo daoAnalyticsBo = new DaoAnalyticsBo();
        daoAnalyticsBo.setDaoId(daoAnalyticsReqDto.getDaoId());
        daoAnalyticsBo.setStartDate(secondStartDate);
        daoAnalyticsBo.setEndDate(firstStartDate);
        daoAnalyticsBo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        List<Work> workList = workService.selecWorkForAnalytics(daoAnalyticsBo);
        // 处理不包含一口价的情况
        if (daoAnalyticsReqDto.getFixedPrice() != null && daoAnalyticsReqDto.getFixedPrice() == 0) {
            workList = workList.stream().filter(v -> !WorkPriceTypeEnum.FIXED_PRICE.getType().equals(v.getPriceType()))
                    .collect(Collectors.toList());
        }
        List<String> times = new ArrayList<>();
        List<String> price = new ArrayList<>();
        List<String> name = new ArrayList<>();
        List<String> image = new ArrayList<>();
        if (workList != null && workList.size() > 0) {
            Map<String,
                    List<Work>> workMap = workList.stream()
                    .collect(Collectors.groupingBy(v -> Instant.ofEpochMilli(Long.parseLong(v.getBlockTime()) * 1000)
                            .atZone(ZoneOffset.UTC).toLocalDate().format(FORMATTER)));
            // 计算开始时间
            for (int i = 0; i < daoAnalyticsReqDto.getDayTime(); i++) {
                List<Work> dateWorkList = workMap.get(lastTimeDateTime.format(FORMATTER));
                if (dateWorkList != null && dateWorkList.size() > 0) {
                    dateWorkList = dateWorkList.stream()
                            .sorted(Comparator.comparing(Work::getBlockTime).thenComparing(Work::getId))
                            .collect(Collectors.toList());
                    for (Work work : dateWorkList) {
                        times.add(work.getBlockTime());
                        price.add(work.getMintedPrice().stripTrailingZeros().toPlainString());
                        // D4A@123/Canvas*123/NFT#123
//                        name.add("D4A@" + work.getDaoNumber() + "/Canvas*" + work.getCanvasNumber() + "/NFT#"
//                                + work.getWorkNumber());
                        name.add(dao.getDaoName() + "." + work.getWorkNumber());
                        image.add(work.getImageUrl());
                    }
                } else {
                    // if (i == 0 || i == (daoAnalyticsReqDto.getDayTime() - 1)) {
                    times.add(String.valueOf(lastTimeDateTime.withHour(0).withMinute(0).withSecond(0).withNano(0)
                            .toInstant(ZoneOffset.UTC).getEpochSecond()));
                    price.add("");
                    name.add("");
                    image.add("");
                    // }

                }
                lastTimeDateTime = lastTimeDateTime.plusDays(1);
                if (lastTimeDateTime.isAfter(localDateTime)) {
                    break;
                }
            }
        }

        daoScatterPlotResVo.setXtime(times);
        daoScatterPlotResVo.setPrice(price);
        daoScatterPlotResVo.setName(name);
        daoScatterPlotResVo.setImage(image);

        result.setData(daoScatterPlotResVo);

        return result;
    }

    /**
     * nft owners top100 1.7
     */
    @PostMapping(value = "/nftTopOwners")
    public Result<DaoTopOwnersResVo> nftTopOwners(@RequestBody DaoReqVo daoReqVo, HttpServletRequest request) {
        Result<DaoTopOwnersResVo> result = new Result<>();
        List<DaoTopOwnersResVo> daoTopOwnersResVoList = new ArrayList<>();
        if (daoReqVo == null || daoReqVo.getDaoId() == null) {
            return result;
        }

        DaoAnalyticsBo daoAnalyticsBo = new DaoAnalyticsBo();
        daoAnalyticsBo.setDaoId(Integer.valueOf(daoReqVo.getDaoId()));
        daoAnalyticsBo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        List<Work> workList = workService.selecWorkForTopOwnersAmount(daoAnalyticsBo);
        int workSize = workList.size();
        if (workSize > 0) {
            Integer nftAmount = workService.selectNftAmounts(daoReqVo.getDaoId());
            List<User> userList = userService
                    .findUsersByUserAddress(workList.stream().map(Work::getOwnerAddress).collect(Collectors.toList()));
            Map<String, User> map = userList.stream().collect(Collectors.toMap(User::getUserAddress, v -> v));

            for (Work work : workList) {
                DaoTopOwnersResVo daoTopOwnersResVo = new DaoTopOwnersResVo();
                User user = map.get(work.getOwnerAddress());
                if (user != null) {
                    String name = StringUtils.isBlank(user.getUserName())
                            ? user.getUserAddress().substring(2, 8).toUpperCase() : user.getUserName();
                    daoTopOwnersResVo.setName(name);
                    daoTopOwnersResVo.setHeadImg(user.getAvatarAddress());
                    daoTopOwnersResVo.setAddress(user.getUserAddress());

                    if (user.getIsContract() != null) {
                        daoTopOwnersResVo.setIsContract(user.getIsContract());
                    } else {
                        Result<Boolean> resultBoolean = iSubscriptionService
                                .ethGetCodeCheckUserAddress(ProtoDaoConstant.netWork, work.getOwnerAddress());
                        if (resultBoolean != null && resultBoolean.getData()) {
                            daoTopOwnersResVo.setIsContract(0);
                        } else {
                            daoTopOwnersResVo.setIsContract(1);
                        }
                    }
                } else {
                    // 判断是否为合约地址
                    daoTopOwnersResVo.setName(work.getOwnerAddress().substring(2, 8).toUpperCase());
                    daoTopOwnersResVo.setAddress(work.getOwnerAddress());
                    // Random random = new Random();
                    SecureRandom random = new SecureRandom(); // Compliant for security-sensitive use cases
                    byte[] bytes = new byte[20];
                    random.nextBytes(bytes);

                    int i = random.nextInt(32) + 1;
                    String avatar = String.format(headImage, i);
                    daoTopOwnersResVo.setHeadImg(avatar);
                    Result<Boolean> resultBoolean = iSubscriptionService
                            .ethGetCodeCheckUserAddress(ProtoDaoConstant.netWork, work.getOwnerAddress());
                    if (resultBoolean != null && resultBoolean.getData()) {
                        daoTopOwnersResVo.setIsContract(0);
                    } else {
                        daoTopOwnersResVo.setIsContract(1);
                    }
                }

                daoTopOwnersResVo.setAmount(work.getWorkAmount());
                daoTopOwnersResVo.setRatio(
                        new BigDecimal(work.getWorkAmount()).divide(new BigDecimal(nftAmount), 4, RoundingMode.HALF_UP)
                                .multiply(new BigDecimal("100")).stripTrailingZeros());
                daoTopOwnersResVoList.add(daoTopOwnersResVo);
            }

        }
        result.setDataList(daoTopOwnersResVoList);
        return result;
    }

    /**
     * Owner Distribution 1.7
     */
    @PostMapping(value = "/ownerDistribution")
    public Result<DaoRangeResVo> ownerDistribution(@RequestBody DaoReqVo daoReqVo, HttpServletRequest request) {
        Result<DaoRangeResVo> result = new Result<>();
        result.setDataList(new ArrayList<>());
        Set<DaoRangeResVo> daoPriceRangeResVoList = new HashSet<>();
        if (daoReqVo == null || daoReqVo.getDaoId() == null) {
            return result;
        }
        DaoAnalyticsBo daoAnalyticsBo = new DaoAnalyticsBo();
        daoAnalyticsBo.setDaoId(Integer.valueOf(daoReqVo.getDaoId()));
        daoAnalyticsBo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        List<Work> workList = workService.selecWorkForOwnersAmount(daoAnalyticsBo);
        Map<Integer, List<Work>> map = workList.stream().collect(Collectors.groupingBy(Work::getWorkAmount));

        // Owner的总数
        BigDecimal workSize = new BigDecimal(workList.size());
        // 拥有最少的人有多少个
        int minWorkAmount = 0;
        int maxWorkAmount = 0;
        if (workList.size() > 0) {
            // 获取最大范围
            maxWorkAmount = workList.stream().mapToInt(Work::getWorkAmount).max().getAsInt();
            minWorkAmount = workList.stream().mapToInt(Work::getWorkAmount).min().getAsInt();
            BigDecimal workAmountDecimal = new BigDecimal(String.valueOf(maxWorkAmount));

            int twoAmount = 0;
            int sixAmount = 0;
            int sevenAmount = 0;
            int eightAmount = 0;
            DaoRangeResVo daoRangeResVoTwo = new DaoRangeResVo();
            DaoRangeResVo daoRangeResVoSix = new DaoRangeResVo();
            DaoRangeResVo daoRangeResVoSeven = new DaoRangeResVo();
            DaoRangeResVo daoRangeResVoEight = new DaoRangeResVo();
            log.info("[ownerDistribution] maxWorkAmount:{} ", maxWorkAmount);
            for (Integer i : map.keySet()) {
                List<Work> works = map.get(i);
                if (works != null && works.size() > 0) {
                    switch (i) {
                        case 1:
                            log.info("[ownerDistribution] case 1");
                            DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
                            daoRangeResVo.setRange("1 item");
                            daoRangeResVo.setRatio(new BigDecimal(works.stream().mapToInt(Work::getWorkAmount).sum())
                                    .divide(workSize, 4, RoundingMode.HALF_UP)
                                    .multiply(new BigDecimal("100").stripTrailingZeros()));
                            daoRangeResVo.setOrder(1);
                            daoPriceRangeResVoList.add(daoRangeResVo);
                            break;
                        case 2:
                        case 3:
                        case 4:
                        case 5:
                            log.info("[ownerDistribution] case 2-5");
                            daoRangeResVoTwo.setRange("2-5 item");
                            twoAmount += works.size();
                            daoRangeResVoTwo
                                    .setRatio(new BigDecimal(twoAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                            .multiply(new BigDecimal("100")).stripTrailingZeros());
                            daoRangeResVoTwo.setOrder(2);
                            daoPriceRangeResVoList.add(daoRangeResVoTwo);
                            break;
                        default:

                            if (workAmountDecimal.compareTo(new BigDecimal("6")) == 0 && i == 6) {
                                sixAmount += works.size();
                                daoRangeResVoSix.setRange("6+ item");
                                daoRangeResVoSix
                                        .setRatio(new BigDecimal(sixAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                                .multiply(new BigDecimal("100")).stripTrailingZeros());
                                daoRangeResVoSix.setOrder(3);
                                daoPriceRangeResVoList.add(daoRangeResVoSix);
                                break;
                            } else if (workAmountDecimal.compareTo(new BigDecimal("7")) == 0 && i == 6) {
                                sixAmount += works.size();
                                daoRangeResVoSix.setRange("6 item");
                                daoRangeResVoSix
                                        .setRatio(new BigDecimal(sixAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                                .multiply(new BigDecimal("100")).stripTrailingZeros());
                                daoRangeResVoSix.setOrder(3);
                                daoPriceRangeResVoList.add(daoRangeResVoSix);
                            } else {
                                if (i >= 6) {
                                    // 6item至6+【（x-5）/3】item
                                    int x = 6 + (maxWorkAmount - 5) / 3;
                                    if (x > 6) {
                                        daoRangeResVoSix.setRange("6-" + x + " item");
                                    }
                                    if (i >= 6 && i <= x) {
                                        sixAmount += works.size();
                                        daoRangeResVoSix.setRatio(
                                                new BigDecimal(sixAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                                        .multiply(new BigDecimal("100")).stripTrailingZeros());
                                        daoRangeResVoSix.setOrder(3);
                                        daoPriceRangeResVoList.add(daoRangeResVoSix);
                                    } else {
                                        if (workAmountDecimal.compareTo(new BigDecimal("7")) == 0) {
                                            daoRangeResVoSix.setRange("6 item");
                                        }
                                        if (sixAmount == 0) {
                                            daoRangeResVoSix.setRatio(BigDecimal.ZERO);
                                            daoRangeResVoSix.setOrder(3);
                                            daoPriceRangeResVoList.add(daoRangeResVoSix);
                                        }
                                    }
                                }

                            }

                            if (workAmountDecimal.compareTo(new BigDecimal("7")) == 0 && i == 7) {
                                sevenAmount += works.size();
                                daoRangeResVoSeven.setRange("7+ item");
                                daoRangeResVoSeven
                                        .setRatio(new BigDecimal(sevenAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                                .multiply(new BigDecimal("100")).stripTrailingZeros());
                                daoRangeResVoSeven.setOrder(4);
                                daoPriceRangeResVoList.add(daoRangeResVoSeven);
                                break;
                            } else {
                                if (i >= 7) {
                                    // 6+【（x-5）/3】+1 item至6+2✖️【（x-5）/3】+1 item
                                    int x = 6 + (maxWorkAmount - 5) / 3 + 1;
                                    int y = 6 + 2 * ((maxWorkAmount - 5) / 3) + 1;
                                    daoRangeResVoSeven.setRange(x + "-" + y + " item");
                                    if (i >= x && i <= y) {
                                        sevenAmount += works.size();
                                        daoRangeResVoSeven.setRatio(
                                                new BigDecimal(sevenAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                                        .multiply(new BigDecimal("100")).stripTrailingZeros());
                                        daoRangeResVoSeven.setOrder(4);
                                        daoPriceRangeResVoList.add(daoRangeResVoSeven);
                                    } else {
                                        if (sevenAmount == 0) {
                                            daoRangeResVoSeven.setOrder(4);
                                            daoRangeResVoSeven.setRatio(BigDecimal.ZERO);
                                            daoPriceRangeResVoList.add(daoRangeResVoSeven);
                                        }
                                    }
                                }

                            }

                            if (workAmountDecimal.compareTo(new BigDecimal("8")) >= 0) {
                                // {6+2✖️【（x-5）/3】+2}+item
                                int x = 6 + 2 * ((maxWorkAmount - 5) / 3) + 2;
                                if (i >= x) {
                                    eightAmount += works.size();
                                    daoRangeResVoEight.setRange(x + "+ item");
                                    daoRangeResVoEight
                                            .setRatio(new BigDecimal(eightAmount).divide(workSize, 4, RoundingMode.HALF_UP)
                                                    .multiply(new BigDecimal("100")).stripTrailingZeros());
                                    daoRangeResVoEight.setOrder(5);
                                    daoPriceRangeResVoList.add(daoRangeResVoEight);
                                } else {
                                    if (eightAmount == 0) {
                                        daoRangeResVoEight.setRange(x + "+ item");
                                        daoRangeResVoEight.setOrder(5);
                                        daoRangeResVoEight.setRatio(BigDecimal.ZERO);
                                        daoPriceRangeResVoList.add(daoRangeResVoEight);
                                    }
                                }
                            }
                    }
                }
            }
        }

        if (minWorkAmount == 0 || minWorkAmount > 1) {
            DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
            daoRangeResVo.setRange("1 item");
            daoRangeResVo.setRatio(BigDecimal.ZERO);
            daoRangeResVo.setOrder(1);
            daoPriceRangeResVoList.add(daoRangeResVo);
        }

        if (minWorkAmount <= 1 || minWorkAmount > FIVE) {
            DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
            daoRangeResVo.setRange("2-5 item");
            daoRangeResVo.setRatio(BigDecimal.ZERO);
            daoRangeResVo.setOrder(2);
            daoPriceRangeResVoList.add(daoRangeResVo);
        }
        if (minWorkAmount < SIX && maxWorkAmount < SIX) {
            DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
            daoRangeResVo.setRange("6+ item");
            daoRangeResVo.setRatio(BigDecimal.ZERO);
            daoRangeResVo.setOrder(3);
            daoPriceRangeResVoList.add(daoRangeResVo);
        }

        DaoRangeResVo daoRangeResVo = new DaoRangeResVo();
        daoRangeResVo.setAmount(workSize.toPlainString());
        result.setData(daoRangeResVo);

        if (workSize.compareTo(BigDecimal.ZERO) > 0) {
            result.setDataList(daoPriceRangeResVoList.stream().sorted(Comparator.comparing(DaoRangeResVo::getOrder))
                    .collect(Collectors.toList()));
        }

        return result;
    }

    /**
     * NFT Owners Quantity NFT拥有者的数量正常在表格中显示 1.7
     */
    @PostMapping(value = "/nftOwnersQuantity")
    public Result<DaoFoldLineResVo> nftOwnersQuantity(@RequestBody DaoAnalyticsReqVo daoAnalyticsReqDto,
                                                      HttpServletRequest request) {
        Result<DaoFoldLineResVo> result = new Result<>();
        DaoFoldLineResVo daoFoldLineResVo = new DaoFoldLineResVo();
        result.setData(daoFoldLineResVo);
        if (daoAnalyticsReqDto == null || daoAnalyticsReqDto.getDaoId() == null
                || daoAnalyticsReqDto.getDayTime() == null) {
            return result;
        }
        // 查询dao，获取dao的开始时间
        Dao dao = daoService.getById(daoAnalyticsReqDto.getDaoId());
        if (dao == null || DaoStatusEnum.NOT_STARTED.getStatus().equals(dao.getDaoStatus())) {
            return result;
        }
        LocalDate daoStartDate = dao.getDaoStartDate();

        LocalDate localDate = LocalDate.now();
        String endDate = localDate.format(FORMATTER);
        // 计算开始时间
        localDate = localDate.minusDays(daoAnalyticsReqDto.getDayTime());
        if (localDate.isBefore(daoStartDate)) {
            localDate = daoStartDate;
        }
        String firstStartDate = localDate.format(FORMATTER);
        log.info("[daoStatistics] firstStartDate:{} endDate:{}", firstStartDate, endDate);
        int lastWorkAmount = 0;

        List<Long> times = new ArrayList<>();
        List<BigDecimal> volume = new ArrayList<>();
        List<BigDecimal> price = new ArrayList<>();

        for (int i = 0; i <= daoAnalyticsReqDto.getDayTime(); i++) {
            int nftOwners =
                    workService.selectNftOwnersByEndDate(daoAnalyticsReqDto.getDaoId() + "", localDate.format(FORMATTER));
            log.info("[daoStatistics] localDate:{} nftOwners:{} ", localDate.format(FORMATTER), nftOwners);
            if (!localDate.equals(daoStartDate) && localDate.format(FORMATTER).equals(firstStartDate)) {
                lastWorkAmount = nftOwners;
                localDate = localDate.plusDays(1);
                continue;
            }
            times.add(localDate.atStartOfDay().toEpochSecond(ZoneOffset.UTC));
            volume.add(new BigDecimal(String.valueOf(nftOwners)));
            price.add(new BigDecimal(String.valueOf(nftOwners - lastWorkAmount)));

            lastWorkAmount = nftOwners;
            localDate = localDate.plusDays(1);
            if (localDate.format(FORMATTER).compareTo(endDate) > 0) {
                break;
            }
        }

        daoFoldLineResVo.setTimes(times);
        daoFoldLineResVo.setVolume(volume);
        daoFoldLineResVo.setPrice(price);

        if (volume.size() > 0) {
            daoFoldLineResVo.setMaxVolume(volume.stream().max(Comparator.comparing(BigDecimal::floatValue)).get());
        }
        if (price.size() > 0) {
            daoFoldLineResVo.setMaxPrice(price.stream().max(Comparator.comparing(BigDecimal::floatValue)).get());
        }

        if (daoFoldLineResVo.getMaxPrice().compareTo(BigDecimal.ZERO) == 0
                && daoFoldLineResVo.getMaxVolume().compareTo(BigDecimal.ZERO) == 0) {
            daoFoldLineResVo.setTimes(new ArrayList<>());
            daoFoldLineResVo.setVolume(new ArrayList<>());
            daoFoldLineResVo.setPrice(new ArrayList<>());
        }

        return result;
    }

    /**
     * DAO Mint NFT Distribution (Canvas) Canvas下NFT占DAO NFT数量百分比 1.7
     */
    @PostMapping(value = "/nftDistribution")
    public Result<DaoProportionResVo> canvasNftDistribution(@RequestBody DaoAnalyticsReqVo daoAnalyticsReqDto,
                                                            HttpServletRequest request) {
        Result<DaoProportionResVo> result = new Result<>();
        List<DaoProportionResVo> daoProportionResVoList = new ArrayList<>();
        if (daoAnalyticsReqDto == null || daoAnalyticsReqDto.getDaoId() == null
                || daoAnalyticsReqDto.getDayTime() == null) {
            result.setDataList(daoProportionResVoList);
            return result;
        }
        DaoAnalyticsBo daoAnalyticsBo = buildFirstDaoAnalyticsBo(daoAnalyticsReqDto);
        log.info("[canvasNftDistribution] daoAnalyticsBo:{}", JacksonUtil.obj2json(daoAnalyticsBo));
        // 查询最近7天
        List<Work> firstWorkList = workService.selecWorkForAnalytics(daoAnalyticsBo);
        // 比例相同时按Canvas创建时间降序
        Map<Integer, List<Work>> workMap = firstWorkList.stream()
                .sorted(Comparator.comparing(Work::getCanId).reversed()).collect(Collectors.groupingBy(Work::getCanId));
        int color = 0;
        for (Integer canId : workMap.keySet()) {
            List<Work> canIdWorks = workMap.get(canId);
            Canvas canvas = canvasService.getById(canId);
            DaoProportionResVo daoProportionResVo = new DaoProportionResVo();
            daoProportionResVo.setCanvasId(canvas.getId());
            daoProportionResVo.setCanvasName(canvas.getCanvasName());
            daoProportionResVo.setCanvasUrl(WebConfigurer.allowedDomain1 + "/canvases?type=nfts&id=" + canvas.getId());
            daoProportionResVo.setNftAmount(canIdWorks.size());
            daoProportionResVo.setRatio(new BigDecimal(canIdWorks.size()).divide(new BigDecimal(firstWorkList.size()),
                    4, RoundingMode.HALF_UP));
            daoProportionResVo.setColor(COLOR_LIST.get(color));
            color = (color == 9) ? 0 : color + 1;
            daoProportionResVoList.add(daoProportionResVo);
        }
        result.setDataList(daoProportionResVoList);
        return result;
    }

    /**
     * DAO NFT Trading Volume Rate (Canvas) Canvas铸造金额占DAO总铸造金额的百分比 1.7
     */
    @PostMapping(value = "/ntvrDistribution")
    public Result<DaoProportionResVo> canvasNtvrDistribution(@RequestBody DaoAnalyticsReqVo daoAnalyticsReqDto,
                                                             HttpServletRequest request) {
        Result<DaoProportionResVo> result = new Result<>();
        List<DaoProportionResVo> daoProportionResVoList = new ArrayList<>();
        if (daoAnalyticsReqDto == null || daoAnalyticsReqDto.getDaoId() == null
                || daoAnalyticsReqDto.getDayTime() == null) {
            return result;
        }
        DaoAnalyticsBo daoAnalyticsBo = buildFirstDaoAnalyticsBo(daoAnalyticsReqDto);
        log.info("[canvasNtvrDistribution] daoAnalyticsBo:{}", JacksonUtil.obj2json(daoAnalyticsBo));
        // 查询最近7天
        List<Work> firstWorkList = workService.selecWorkForAnalytics(daoAnalyticsBo);
        BigDecimal totalMintPrice =
                firstWorkList.stream().map(Work::getMintedPrice).reduce(BigDecimal.ZERO, BigDecimal::add);
        // 比例相同时按Canvas创建时间降序降序
        Map<Integer, List<Work>> workMap = firstWorkList.stream()
                .sorted(Comparator.comparing(Work::getCanId).reversed()).collect(Collectors.groupingBy(Work::getCanId));
        int color = 0;
        for (Integer canId : workMap.keySet()) {
            List<Work> canIdWorks = workMap.get(canId);
            BigDecimal canIdMintPrice =
                    canIdWorks.stream().map(Work::getMintedPrice).reduce(BigDecimal.ZERO, BigDecimal::add);
            Canvas canvas = canvasService.getById(canId);
            DaoProportionResVo daoProportionResVo = new DaoProportionResVo();
            daoProportionResVo.setCanvasId(canvas.getId());
            daoProportionResVo.setCanvasName(canvas.getCanvasName());
            daoProportionResVo.setCanvasUrl(WebConfigurer.allowedDomain1 + "/canvases?type=nfts&id=" + canvas.getId());
            if (totalMintPrice.compareTo(BigDecimal.ZERO) == 0) {
                daoProportionResVo.setRatio(new BigDecimal(String.valueOf(canIdWorks.size())).divide(new BigDecimal(String.valueOf(firstWorkList.size())), 4, RoundingMode.HALF_UP));
            } else {
                daoProportionResVo.setRatio(canIdMintPrice.divide(totalMintPrice, 4, RoundingMode.HALF_UP));
            }
            daoProportionResVoList.add(daoProportionResVo);
            daoProportionResVo.setColor(COLOR_LIST.get(color));
            color = (color == 9) ? 0 : color + 1;
        }
        result.setDataList(daoProportionResVoList);

        return result;
    }

    /**
     * 1.4 返回最近七天的eth变化曲线
     */
    @PostMapping(value = "/assetpool/eth")
    public Result<AssetPoolEthResVo> ethFoldLine(@RequestBody DaoIdReqVo daoIdReqVo,
                                                 HttpServletRequest request) {
        Result<AssetPoolEthResVo> result = new Result<>();
        if (daoIdReqVo == null || daoIdReqVo.getDaoId() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "daoId is null.");
            return result;
        }
        Dao dao = daoService.getById(daoIdReqVo.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "dao not exists.");
            return result;
        }
        Timestamp today = DateUtil.getBeginOfToday();
        long sevenDayBeginHour = DateUtil.getTimestampAfterDay(today, -6).getTime() / 1000;
        long todayBeginHour = DateUtil.getTimestampAfterDay(today, 0).getTime() / 1000;

        List<DaoDailyStatistics> yesterdayDaoDailyStatisticList = daoDailyStatisticsService.selectDaoDailyCompleteByDaoIdAndRecordTime(Integer.valueOf(daoIdReqVo.getDaoId()), sevenDayBeginHour);

        List<Long> time = new ArrayList<Long>();
        List<String> totalAmount = new ArrayList<String>();
        List<String> incomes = new ArrayList<String>();
        List<String> costs = new ArrayList<String>();
        List<String> changes = new ArrayList<String>();

        BigDecimal maxTotalAmount = BigDecimal.ZERO;
        BigDecimal maxIncomes = BigDecimal.ZERO;
        BigDecimal maxCosts = BigDecimal.ZERO;
        BigDecimal maxChanges = BigDecimal.ZERO;

        //没有考虑dao开始时间不足7天的情况
        for (DaoDailyStatistics daoDailyStatistics : yesterdayDaoDailyStatisticList) {
            BigDecimal assetPoolTotalAmount = daoDailyStatistics.getAssetPoolEthTotalAmount();
            BigDecimal assetPoolIncome = daoDailyStatistics.getAssetPoolEthIncome();
            BigDecimal assetPoolCost = daoDailyStatistics.getAssetPoolEthCost();
            BigDecimal assetPoolVariation = daoDailyStatistics.getAssetPoolEthVariation();
            time.add(daoDailyStatistics.getRecordTime());
            totalAmount.add(assetPoolTotalAmount.stripTrailingZeros().toPlainString());
            incomes.add(assetPoolIncome.stripTrailingZeros().toPlainString());
            costs.add(assetPoolCost.stripTrailingZeros().toPlainString());
            changes.add(assetPoolVariation.stripTrailingZeros().toPlainString());
            if (assetPoolTotalAmount.compareTo(maxTotalAmount) > 0) {
                maxTotalAmount = assetPoolTotalAmount;
            }
            if (assetPoolIncome.compareTo(maxIncomes) > 0) {
                maxIncomes = assetPoolIncome;
            }
            if (assetPoolCost.compareTo(maxCosts) > 0) {
                maxCosts = assetPoolCost;
            }
            if (assetPoolVariation.compareTo(maxChanges) > 0) {
                maxChanges = assetPoolVariation;
            }
        }
        //当日的,
//        DaoDailyStatistics yesterdayDaoDailyStatistics = new DaoDailyStatistics();
//        if (!yesterdayDaoDailyStatisticList.isEmpty()) {
//            yesterdayDaoDailyStatistics = yesterdayDaoDailyStatisticList.get(yesterdayDaoDailyStatisticList.size() - 1);
//        } else {
//            yesterdayDaoDailyStatistics.setAssetPoolEthTotalAmount(BigDecimal.ZERO);
//        }
        BigDecimal ethBalance = commonService.getInputToken(dao); //commonService.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals());

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        Date todayNow = new Date();
        Date tomorrow = DateUtil.addDay(todayNow, 1);
        String todayStr = sdf.format(todayNow);
        String tomorrowStr = sdf.format(tomorrow);


        BigDecimal assetPoolEthIncome = commonService.searchEthIncome(todayStr, tomorrowStr, dao.getFeePool());
        BigDecimal costEth = commonService.searchEthCost(todayStr, tomorrowStr, dao.getFeePool());
        BigDecimal assetPoolEthVariation = assetPoolEthIncome.subtract(costEth);

//        List<DaoDrbStatistics> daoDrbStatisticsList = daoDrbStatisticsService.selectByDaoIdAndRecordTime(dao.getId(), todayBeginHour);
//        if (!daoDrbStatisticsList.isEmpty()) {
//            costEth = daoDrbStatisticsList.stream().map(DaoDrbStatistics::getAssetPoolEthCost).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add);
//        }
        //查询当前dao的assetPool的eth余额和token余额

        time.add(todayBeginHour);
        totalAmount.add(ethBalance.stripTrailingZeros().toPlainString());
        incomes.add(assetPoolEthIncome.stripTrailingZeros().toPlainString());
        costs.add(costEth.stripTrailingZeros().toPlainString());
        changes.add(assetPoolEthVariation.stripTrailingZeros().toPlainString());
        if (ethBalance.compareTo(maxTotalAmount) > 0) {
            maxTotalAmount = ethBalance;
        }
        if (assetPoolEthIncome.compareTo(maxIncomes) > 0) {
            maxIncomes = assetPoolEthIncome;
        }
        if (costEth.compareTo(maxCosts) > 0) {
            maxCosts = costEth;
        }
        if (assetPoolEthVariation.compareTo(maxChanges) > 0) {
            maxChanges = assetPoolEthVariation;
        }


        AssetPoolEthResVo assetPoolEthResVo = new AssetPoolEthResVo();
        assetPoolEthResVo.setTime(time);
        assetPoolEthResVo.setTotalAmount(totalAmount);
        assetPoolEthResVo.setIncomes(incomes);
        assetPoolEthResVo.setCosts(costs);
        assetPoolEthResVo.setChanges(changes);
        assetPoolEthResVo.setMaxTotalAmount(maxTotalAmount.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxIncomes(maxIncomes.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxCosts(maxCosts.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxChanges(maxChanges.max(maxCosts).max(maxIncomes).stripTrailingZeros().toPlainString()); // maxChanges.stripTrailingZeros().toPlainString()  // 比较差值和收入，支出最大值

        result.setData(assetPoolEthResVo);
        return result;
    }

    /**
     * 1.4 返回最近七天的token变化曲线
     */
    @PostMapping(value = "/assetpool/token")
    public Result<AssetPoolEthResVo> tokenFoldLine(@RequestBody DaoIdReqVo daoIdReqVo,
                                                   HttpServletRequest request) {
        Result<AssetPoolEthResVo> result = new Result<>();
        if (daoIdReqVo == null || daoIdReqVo.getDaoId() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "daoId is null.");
            return result;
        }
        Dao dao = daoService.getById(daoIdReqVo.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "dao not exists.");
            return result;
        }
        Timestamp today = DateUtil.getBeginOfToday();
        long sevenDayBeginHour = DateUtil.getTimestampAfterDay(today, -6).getTime() / 1000;
        long todayBeginHour = DateUtil.getTimestampAfterDay(today, 0).getTime() / 1000;

        List<DaoDailyStatistics> yesterdayDaoDailyStatisticList = daoDailyStatisticsService.selectDaoDailyCompleteByDaoIdAndRecordTime(Integer.valueOf(daoIdReqVo.getDaoId()), sevenDayBeginHour);

        List<Long> time = new ArrayList<Long>();
        List<String> totalAmount = new ArrayList<String>();
        List<String> incomes = new ArrayList<String>();
        List<String> costs = new ArrayList<String>();
        List<String> changes = new ArrayList<String>();

        BigDecimal maxTotalAmount = BigDecimal.ZERO;
        BigDecimal maxIncomes = BigDecimal.ZERO;
        BigDecimal maxCosts = BigDecimal.ZERO;
        BigDecimal maxChanges = BigDecimal.ZERO;

        for (DaoDailyStatistics daoDailyStatistics : yesterdayDaoDailyStatisticList) {
            BigDecimal assetPoolTotalAmount = daoDailyStatistics.getAssetPoolTokenTotalAmount();
            BigDecimal assetPoolIncome = daoDailyStatistics.getAssetPoolTokenIncome();
            BigDecimal assetPoolCost = daoDailyStatistics.getAssetPoolTokenCost();
            BigDecimal assetPoolVariation = daoDailyStatistics.getAssetPoolTokenVariation();
            time.add(daoDailyStatistics.getRecordTime());
            totalAmount.add(assetPoolTotalAmount.stripTrailingZeros().toPlainString());
            incomes.add(assetPoolIncome.stripTrailingZeros().toPlainString());
            costs.add(assetPoolCost.stripTrailingZeros().toPlainString());
            changes.add(assetPoolVariation.stripTrailingZeros().toPlainString());
            if (assetPoolTotalAmount.compareTo(maxTotalAmount) > 0) {
                maxTotalAmount = assetPoolTotalAmount;
            }
            if (assetPoolIncome.compareTo(maxIncomes) > 0) {
                maxIncomes = assetPoolIncome;
            }
            if (assetPoolCost.compareTo(maxCosts) > 0) {
                maxCosts = assetPoolCost;
            }
            if (assetPoolVariation.compareTo(maxChanges) > 0) {
                maxChanges = assetPoolVariation;
            }
        }
        //当日的,
//        DaoDailyStatistics yesterdayDaoDailyStatistics = new DaoDailyStatistics();
//        if (!yesterdayDaoDailyStatisticList.isEmpty()) {
//            yesterdayDaoDailyStatistics = yesterdayDaoDailyStatisticList.get(yesterdayDaoDailyStatisticList.size() - 1);
//        } else {
//            yesterdayDaoDailyStatistics.setAssetPoolTokenTotalAmount(BigDecimal.ZERO);
//        }

        BigDecimal tokenBalance = commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        Date todayNow = new Date();
        Date tomorrow = DateUtil.addDay(todayNow, 1);
        String todayStr = sdf.format(todayNow);
        String tomorrowStr = sdf.format(tomorrow);


        BigDecimal assetPoolTokenIncome = commonService.searchTokenIncome(todayStr, tomorrowStr, dao.getFeePool(), dao.getErc20Token());
        BigDecimal costToken = commonService.searchTokeCost(todayStr, tomorrowStr, dao.getFeePool(), dao.getErc20Token());

        BigDecimal assetPoolTokenVariation = assetPoolTokenIncome.subtract(costToken);
//        List<DaoDrbStatistics> daoDrbStatisticsList = daoDrbStatisticsService.selectByDaoIdAndRecordTime(dao.getId(), todayBeginHour);
//        if (!daoDrbStatisticsList.isEmpty()) {
//            costToken = daoDrbStatisticsList.stream().map(DaoDrbStatistics::getAssetPoolTokenCost).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add);
//        }
        //查询当前dao的assetPool的eth余额和token余额
//        BigDecimal ethBalance = commonService.ethBalanceOf(dao.getFeePool());
//        if (yesterdayDaoDailyStatistics != null && yesterdayDaoDailyStatistics.getAssetPoolTokenTotalAmount() != null) {
//            assetPoolTokenVariation = tokenBalance.subtract(yesterdayDaoDailyStatistics.getAssetPoolTokenTotalAmount());
//        } else {
//            assetPoolTokenVariation = tokenBalance;
//        }
//        assetPoolTokenIncome = assetPoolTokenVariation.add(costToken);

        time.add(todayBeginHour);
        totalAmount.add(tokenBalance.stripTrailingZeros().toPlainString());
        incomes.add(assetPoolTokenIncome.stripTrailingZeros().toPlainString());
        costs.add(costToken.stripTrailingZeros().toPlainString());
        changes.add(assetPoolTokenVariation.stripTrailingZeros().toPlainString());
        if (tokenBalance.compareTo(maxTotalAmount) > 0) {
            maxTotalAmount = tokenBalance;
        }
        if (assetPoolTokenIncome.compareTo(maxIncomes) > 0) {
            maxIncomes = assetPoolTokenIncome;
        }
        if (costToken.compareTo(maxCosts) > 0) {
            maxCosts = costToken;
        }
        if (assetPoolTokenVariation.compareTo(maxChanges) > 0) {
            maxChanges = assetPoolTokenVariation;
        }


        AssetPoolEthResVo assetPoolEthResVo = new AssetPoolEthResVo();
        assetPoolEthResVo.setTime(time);
        assetPoolEthResVo.setTotalAmount(totalAmount);
        assetPoolEthResVo.setIncomes(incomes);
        assetPoolEthResVo.setCosts(costs);
        assetPoolEthResVo.setChanges(changes);
        assetPoolEthResVo.setMaxTotalAmount(maxTotalAmount.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxIncomes(maxIncomes.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxCosts(maxCosts.stripTrailingZeros().toPlainString());
        assetPoolEthResVo.setMaxChanges(maxChanges.max(maxCosts).max(maxIncomes).stripTrailingZeros().toPlainString()); // maxChanges.stripTrailingZeros().toPlainString() // 比较差值和收入，支出最大值

        result.setData(assetPoolEthResVo);

        return result;
    }


    /**
     * 1.4 dao的eth的分配比例
     */
    @PostMapping(value = "/allocation/info")
    public Result<DaoAllocationInfoResVo> daoAllocationInfo(@RequestBody DaoIdReqVo daoIdReqVo,
                                                            HttpServletRequest request) {
        Result<DaoAllocationInfoResVo> result = new Result<>();

        DaoAllocationInfoResVo daoAllocationInfoResVo = new DaoAllocationInfoResVo();
        result.setData(daoAllocationInfoResVo);
        Dao dao = daoService.getById(daoIdReqVo.getDaoId());
        if (dao == null) {
            return result;
        }


        //查询当前dao分配给其他dao的分配信息
        List<DaoAllocationVo> daoTokenAllocationVos = new ArrayList<>();
        List<DaoAllocationVo> daoEthAllocationVos = new ArrayList<>();

        List<DaoAllocationStrategy> daoAllocationStrategyList = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
        daoTokenAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
        daoEthAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1).map(DaoAllocationVo::transfer).collect(Collectors.toList());


        if (TrueOrFalseEnum.FALSE.getStatus().equals(dao.getTopupMode())) {
            daoAllocationInfoResVo.setAllocationTokenToOtherDao(daoTokenAllocationVos);
            daoAllocationInfoResVo.setAllocationEthToOtherDao(daoEthAllocationVos);
        } else {
            daoAllocationInfoResVo.setAllocationEthToOtherDao(new ArrayList<>());
            daoTokenAllocationVos.forEach(v -> {
                if (DaoRoyaltyTypeEnum.THREE.getType().equals(v.getRoyaltyType())) {
                    v.setRoyaltyProportion(new BigDecimal("0"));
                }
                if (DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())) {
                    v.setRoyaltyProportion(new BigDecimal("100"));
                }
            });
            daoAllocationInfoResVo.setAllocationTokenToOtherDao(daoTokenAllocationVos);

        }

        //查询其他dao分配给当前dao的分配信息
        List<DaoAllocationVo> receivedEthFromOther = new ArrayList<>();
        List<DaoAllocationVo> receivedTokenFromOther = new ArrayList<>();

        List<DaoAllocationStrategy> daoAllocationStrategyToProject = daoAllocationStrategyService.selectByProjectIdAndType(dao.getProjectId(), null);
        receivedTokenFromOther = daoAllocationStrategyToProject.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transferOriginProject).collect(Collectors.toList());
        receivedEthFromOther = daoAllocationStrategyToProject.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transferOriginProject).collect(Collectors.toList());


        daoAllocationInfoResVo.setReceivedTokenFromOther(receivedTokenFromOther);
        daoAllocationInfoResVo.setReceivedEthFromOther(receivedEthFromOther);

        //dao 内部分配策略
        DaoRoyaltyToken daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
        DaoEthRoyaltyToken daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);

        DaoRoyaltyToken daoRoyaltyEth = new DaoRoyaltyToken();
        daoRoyaltyEth.setDaoReward(daoEthRoyaltyToken.getDaoCreatorETHReward());
        daoRoyaltyEth.setD4aReward(daoEthRoyaltyToken.getD4aReward());
        daoRoyaltyEth.setCanvasReward(daoEthRoyaltyToken.getCanvasCreatorETHReward());
        daoRoyaltyEth.setMinterReward(daoEthRoyaltyToken.getMinterETHReward());


        if (TrueOrFalseEnum.FALSE.getStatus().equals(dao.getTopupMode())) {
            daoAllocationInfoResVo.setRoyaltyToken(daoRoyaltyToken);
        } else {
            daoRoyaltyToken.setDaoReward(BigDecimal.ZERO);
            daoRoyaltyToken.setCanvasReward(BigDecimal.ZERO);
            daoRoyaltyToken.setD4aReward(BigDecimal.ZERO);
            daoRoyaltyToken.setMinterReward(new BigDecimal("100"));
            daoAllocationInfoResVo.setRoyaltyToken(daoRoyaltyToken);
        }
        daoAllocationInfoResVo.setEthRoyaltyToken(daoRoyaltyEth);


        //当前区块铸造收益
        CurrentMintWindowInfoVo currentMintWindowInfoVo = new CurrentMintWindowInfoVo();
        WorkCountBo workCountBo = workService.selectDrbNftOwnerCountByDaoId(String.valueOf(dao.getId()), Integer.valueOf(dao.getCurrentRound()));

        currentMintWindowInfoVo.setMinters(workCountBo.getMinters());
        if (StringUtils.isNotBlank(workCountBo.getMintFee())) {
            currentMintWindowInfoVo.setMintFee(workCountBo.getMintFee());
        }
        currentMintWindowInfoVo.setMintedWorks(workCountBo.getMintedWorks());
//        currentMintWindowInfoVo.setBlockRewardToken();
//        currentMintWindowInfoVo.setBlockRewardEth();
//        currentMintWindowInfoVo.setInternalRewardToken();
//        currentMintWindowInfoVo.setInternalRewardEth();


        daoAllocationInfoResVo.setCurrentMintWindowInfoVo(currentMintWindowInfoVo);

        daoAllocationInfoResVo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));
        return result;
    }


    /**
     * 1.12 修改接口 1.4.2 聚合dao的详情页面 （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/info")
    public Result<TogetherDaoDetailVo> togetherDaoInfo(@RequestBody DaoIdReqVo daoIdReqVo,
                                                       HttpServletRequest request) {
        Result<TogetherDaoDetailVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        TogetherDaoDetailVo togetherDaoDetailVo = TogetherDaoDetailVo.transfer(dao);

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(userAddress)) {
            if (dao.getOwnerAddress().equalsIgnoreCase(userAddress)) {
                togetherDaoDetailVo.setModifiable(true);
            }
        }

        // 1.12加入maker信息
        togetherDaoDetailVo.setTogetherDaoMakerVo(transferTogetherDaoMakerVo(dao));

        result.setData(togetherDaoDetailVo);

        return result;

    }


    /**
     * 1.4.2 聚合dao的详情页面 （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/member")
    public Result<TogetherDaoMemberVo> togetherDaoMember(@RequestBody DaoIdReqVo daoIdReqVo,
                                                         HttpServletRequest request) {
        Result<TogetherDaoMemberVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        List<Dao> daoList = daoService.selectByTogetherDaoId(dao.getId() + "");

        List<String> daoOwners = daoList.stream().map(Dao::getOwnerAddress).collect(Collectors.toList());
        List<Integer> daoIds = daoList.stream().map(Dao::getId).collect(Collectors.toList());


        List<Canvas> canvasList = canvasService.listCanvasByDaoIds(daoIds);
        Set<String> canvasOwners = canvasList.stream().map(Canvas::getOwnerAddress).collect(Collectors.toSet());

        List<Work> workList = workService.selectWorksByDaoIds(daoIds);
        Set<String> minters = workList.stream().filter(v -> WorkStatusEnum.CASTED.getStatus().equals(v.getWorkStatus())).map(Work::getMintedAddress).collect(Collectors.toSet());
        Set<String> holders = workList.stream().filter(v -> WorkStatusEnum.CASTED.getStatus().equals(v.getWorkStatus())).map(Work::getOwnerAddress).collect(Collectors.toSet());

        TogetherDaoMemberVo togetherDaoMemberVo = new TogetherDaoMemberVo();
        togetherDaoMemberVo.setStarter(new HashSet<>(daoOwners).size());
        togetherDaoMemberVo.setBuilder(canvasOwners.size());
        togetherDaoMemberVo.setMintter(minters.size());
        togetherDaoMemberVo.setNftHolders(holders.size());

        Integer tokenHolders = daoList.stream().filter(v -> v.getTokenHolders() != null && v.getTokenHolders() > 0).map(Dao::getTokenHolders).findFirst().orElse(0);
        togetherDaoMemberVo.setErc20Holders(tokenHolders);


        result.setData(togetherDaoMemberVo);

        return result;

    }

    /**
     * 1.4.2 这一系列DAO的数量 （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/amount")
    public Result<TogetherDaoAmountVo> togetherDaoAmount(@RequestBody DaoIdReqVo daoIdReqVo,
                                                         HttpServletRequest request) {
        Result<TogetherDaoAmountVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        List<Dao> daoList = daoService.selectByTogetherDaoId(dao.getId() + "");

        TogetherDaoAmountVo togetherDaoAmountVo = new TogetherDaoAmountVo();
        togetherDaoAmountVo.setTotalAmount(daoList.size());
        // 该聚合dao下任意一个dao id  或者 main dao id
        Optional<Dao> daoOptional = daoList.stream().filter(v -> Integer.valueOf(1).equals(v.getIsAncestordao())).findAny();
        daoOptional.ifPresent(value -> togetherDaoAmountVo.setMainDaoId(value.getId()));
        togetherDaoAmountVo.setRunningDaoAmount((int) daoList.stream().filter(v -> DaoStatusEnum.STARTED.getStatus().equals(v.getDaoStatus())).count());
        togetherDaoAmountVo.setEndedDaoAmount((int) daoList.stream().filter(v -> DaoStatusEnum.FINISHED.getStatus().equals(v.getDaoStatus())).count());
        togetherDaoAmountVo.setNotStartedDaoAmount((int) daoList.stream().filter(v -> DaoStatusEnum.NOT_STARTED.getStatus().equals(v.getDaoStatus())).count());

        result.setData(togetherDaoAmountVo);

        return result;

    }


    /**
     * 1.4.2 聚合dao的ERC20信息 （参数daoId为聚合dao的daoID）
     * 1.6 修改获取余额逻辑(在TokenReceivedRecord表中找tokenType=1且project在一系列dao下的总和。。)
     */
    @PostMapping(value = "/togetherDao/token")
    public Result<TogetherDaoTokenVo> togetherDaoToken(@RequestBody DaoIdReqVo daoIdReqVo,
                                                       HttpServletRequest request) {
        Result<TogetherDaoTokenVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao mainDao = null;
        List<Dao> daoList = daoService.selectByTogetherDaoId(dao.getId() + "");

        if (daoList.size() > 0) {
            mainDao = daoList.get(0);
        }
        if (mainDao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        String decimals = commonService.erc20Decimals(dao.getErc20Token());
        if (StringUtils.isBlank(decimals)) {
            log.error("[togetherDaoToken] dao:{} getDecimals error", dao.getId());
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO erc20 is error");
            return result;
        }
        BigDecimal totalSupply = commonService.getErc20TotalSupply(dao.getErc20Token(), Integer.parseInt(decimals)); // dao.getErc20TokenDecimals()

        BigDecimal burnErc20Amount = daoList.stream().map(Dao::getBurnAmount).filter(Objects::nonNull).reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal erc20BalanceOf = daoList.stream().filter(v -> StringUtils.isNotBlank(v.getSubdaoAssetPoolBalance()))
                .map(v -> new BigDecimal(v.getSubdaoAssetPoolBalance())).reduce(BigDecimal.ZERO, BigDecimal::add);
        log.info("totalSupply:{}", totalSupply.toPlainString());
        log.info("burnErc20Amount:{}", burnErc20Amount.toPlainString());
        log.info("erc20BalanceOf:{}", erc20BalanceOf.toPlainString());

        // 查询国库余额信息
//        Integer decimal = null;
//        if (dao.getIsThirdpartyToken().equals(1)){
//            // 如果是外部erc20
//            String decimals20 = commonService.erc20Decimals(dao.getErc20Token());
//            if (StringUtils.isNotBlank(decimals20)){
//                decimal = Integer.valueOf(decimals20);
//            }
//        }

        BigDecimal treasuryBalance = commonService.erc20BalanceOf(dao.getErc20Token(), dao.getTreasuryErc20(), Integer.parseInt(decimals), dao.getInputTokenDecimals());
        log.info("treasuryBalance:{}", treasuryBalance.toPlainString());

        TogetherDaoTokenVo togetherDaoTokenVo = new TogetherDaoTokenVo();
        togetherDaoTokenVo.setErc20Address(mainDao.getErc20Token());
        togetherDaoTokenVo.setTotalSupply(totalSupply.stripTrailingZeros().toPlainString());
        // 1.6 修改，原来的burnErc20Amount计算错误，原来的值在ProtoDaoSchedule.java中赋值，待查看
        //togetherDaoTokenVo.setDaoTokenBalance(totalSupply.subtract(erc20BalanceOf).subtract(burnErc20Amount).subtract(treasuryBalance).stripTrailingZeros().toPlainString());
        //togetherDaoTokenVo.setRedeemAssetPoolEth(commonService.ethBalanceOf(mainDao.getDaoRedeemPool(),mainDao.getInputTokenDecimals()).stripTrailingZeros().toPlainString());
        BigDecimal remainderEth = null;
        if (ProtoDaoConstant.DEFAULT_PAY_CURRENCY_TYPE.equals(dao.getPayCurrencyType())) {
            // 如果支付类型为eth，那就查询eth
            remainderEth = commonService.ethBalanceOf(dao.getDaoRedeemPool(), dao.getInputTokenDecimals());
        } else {
            // 如果支付类型为其他，查询在input token下的余额
            remainderEth = commonService.erc20BalanceOf(dao.getInputTokenAddress(), dao.getDaoRedeemPool(), dao.getInputTokenDecimals(), null);
        }
        togetherDaoTokenVo.setRedeemAssetPoolEth(remainderEth.stripTrailingZeros().toPlainString());
        // togetherDaoTokenVo.setRedeemedErc20Amont(burnErc20Amount.stripTrailingZeros().toPlainString());

//        BigDecimal redeemedErc20Amount = daoList.stream()
//                .map(dao1 -> tokenReceivedRecordService.selectDaoBurnAmount(dao1.getProjectId()))
//                .reduce(BigDecimal.ZERO, BigDecimal::add);

//        BigDecimal redeemedErc20Amount1 = daoList.stream()
//                .map(dao1 -> Optional.ofNullable(tokenReceivedRecordService.selectDaoBurnAmount(dao1.getProjectId()))
//                        .orElse(BigDecimal.ZERO))
//                .reduce(BigDecimal.ZERO, BigDecimal::add);
//        log.info("redeemedErc20Amount1:{}",redeemedErc20Amount1);
        BigDecimal redeemedErc20Amount = tokenReceivedRecordService.selectDaoBurnAmountSum(dao.getId() + "");
        togetherDaoTokenVo.setRedeemedErc20Amont(redeemedErc20Amount.toPlainString());

        togetherDaoTokenVo.setDaoTokenBalance(totalSupply.subtract(erc20BalanceOf).subtract(redeemedErc20Amount).subtract(treasuryBalance).stripTrailingZeros().toPlainString());


        result.setData(togetherDaoTokenVo);

        return result;

    }


    /**
     * 1.4.2 聚合dao的Maker信息 maker的定义为参与过这一系列DAO下的任何一个top-up模式dao的铸造。
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/maker")
    public Result<TogetherDaoMakerVo> togetherDaoMaker(@RequestBody DaoIdReqVo daoIdReqVo,
                                                       HttpServletRequest request) {
        //查询这个dao下有被amount，且数量>0，联查amount-workId的owner的人数去重
        Result<TogetherDaoMakerVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

//        List<Dao> daoList = daoService.selectByTogetherDaoId(dao.getId() + "");
//
////        List<Integer> daoIds = daoList.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getTopupMode())).map(Dao::getId).collect(Collectors.toList());
//        List<Integer> allDaoIds = daoList.stream().map(Dao::getId).collect(Collectors.toList());
//        List<Integer> daoIds = daoList.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getTopupMode())).map(Dao::getId).collect(Collectors.toList());
//
//        List<Work> workList = workService.selectWorksByDaoIds(daoIds);
//        Set<String> minters = workList.stream().filter(v -> WorkStatusEnum.CASTED.getStatus().equals(v.getWorkStatus())).map(Work::getMintedAddress).collect(Collectors.toSet());
//
//        TogetherDaoMakerVo togetherDaoMakerVo = new TogetherDaoMakerVo();
//        togetherDaoMakerVo.setMakerTotalAmount(minters.size());
//
//        List<UserTopupHarvest> userTopupHarvestList = userTopupHarvestService.selectUserTopupHarvestByDaoIds(allDaoIds);
//
//        BigDecimal ethAmount = userTopupHarvestList.stream().filter(v -> v.getEthAmount() != null).map(UserTopupHarvest::getEthAmount).reduce(BigDecimal.ZERO, BigDecimal::add);
//        BigDecimal tokenAmount = userTopupHarvestList.stream().filter(v -> v.getErc20Amount() != null).map(UserTopupHarvest::getErc20Amount).reduce(BigDecimal.ZERO, BigDecimal::add);
//        togetherDaoMakerVo.setNoSpendEthAmount(ethAmount.stripTrailingZeros().toPlainString());
//        togetherDaoMakerVo.setNoSpendTokenAmount(tokenAmount.stripTrailingZeros().toPlainString());
        result.setData(transferTogetherDaoMakerVo(dao));

        return result;

    }


    /**
     * 1.6 聚合dao的Maker信息页面
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/maker/list")
    public Result<TogetherDaoMakerListVo> togetherDaoMakerList(@RequestBody DaoIdReqVo daoIdReqVo,
                                                               HttpServletRequest request) {
        Result<TogetherDaoMakerListVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        List<Dao> daoList = daoService.selectByTogetherDaoId(dao.getId() + "");
        TogetherDaoMakerListVo togetherDaoMakerListVo = TogetherDaoMakerListVo.transForTogetherDaoMakerLis(daoList);
        togetherDaoMakerListVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        togetherDaoMakerListVo.setOwnerAddress(dao.getOwnerAddress());

        // eth 解锁 token 默认比例
        togetherDaoMakerListVo.setEthTransTokenTreasuryRatioDefault(dao.getEthTokenRoyalty().multiply(new BigDecimal("100")));
        togetherDaoMakerListVo.setEthTransTokenWalletRatioDefault(new BigDecimal("100").subtract(togetherDaoMakerListVo.getEthTransTokenTreasuryRatioDefault()));

        // token 解锁 eth 默认比例
        togetherDaoMakerListVo.setTokenTransEthRedeemRatioDefault(dao.getTokenEthRoyalty().multiply(new BigDecimal("100")));
        togetherDaoMakerListVo.setTokenTransEthWalletRatioDefault(new BigDecimal("100").subtract(togetherDaoMakerListVo.getTokenTransEthRedeemRatioDefault()));

        result.setData(togetherDaoMakerListVo);
        return result;
    }

    /**
     * 1.6 聚合dao的Treasury信息
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/treasury")
    public Result<TogetherDaoTreasuryVo> togetherDaoTreasury(@RequestBody DaoIdReqVo daoIdReqVo,
                                                             HttpServletRequest request) {
        Result<TogetherDaoTreasuryVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        dao = daoService.getDaoByProjectId(dao.getProjectId(), DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());

        TogetherDaoTreasuryVo togetherDaoTreasuryVo = new TogetherDaoTreasuryVo();

        // 查询国库余额信息
        Integer decimal = null;
//        if (dao.getIsThirdpartyToken().equals(1)){
        // 如果是外部erc20
        String decimals20 = commonService.erc20Decimals(dao.getErc20Token());
        log.info("获取到到外部20decimal:{}", decimals20);
        if (StringUtils.isNotBlank(decimals20)) {
            decimal = Integer.valueOf(decimals20);
        }
//        }

        BigDecimal tokenBalance = commonService.erc20BalanceOf(dao.getErc20Token(), dao.getTreasuryErc20(), decimal, dao.getInputTokenDecimals());
        togetherDaoTreasuryVo.setTreasuryTotalAmount(tokenBalance);
        result.setData(togetherDaoTreasuryVo);
        return result;
    }


    /**
     * 1.6 聚合dao的Treasury页面信息
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/treasury/info")
    public Result<TogetherDaoTreasuryInfoVo> togetherDaoTreasuryInfo(@RequestBody DaoIdReqVo daoIdReqVo,
                                                                     HttpServletRequest request) {
        Result<TogetherDaoTreasuryInfoVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        dao = daoService.getDaoByProjectId(dao.getProjectId(), DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());
        TogetherDaoTreasuryInfoVo togetherDaoTreasuryInfoVo = new TogetherDaoTreasuryInfoVo();

        // 国库地址和dao的erc20地址
        togetherDaoTreasuryInfoVo.setTreasuryAddress(dao.getTreasuryErc20());
        togetherDaoTreasuryInfoVo.setErc20Address(dao.getErc20Token());
        togetherDaoTreasuryInfoVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        togetherDaoTreasuryInfoVo.setOwnerAddress(dao.getOwnerAddress());
        result.setData(togetherDaoTreasuryInfoVo);
        return result;
    }


    /**
     * 1.6 聚合dao的Treasury页面信息
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/treasury/transaction")
    public ResultList<TogetherDaoTreasuryTransactionVo> togetherDaoTreasuryTransaction(@RequestBody DaoIdParam daoIdParam,
                                                                                       HttpServletRequest request) {
        ResultList<TogetherDaoTreasuryTransactionVo> result = new ResultList<>();
        if (daoIdParam == null || StringUtils.isBlank(daoIdParam.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdParam.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<TreasuryTransaction> iPage = new Page<>(daoIdParam.getPageNo(), daoIdParam.getPageSize());

        Page<TogetherDaoTreasuryTransactionVo> transactionPage = treasuryTransactionService.getTreasuryTransactionList(iPage, dao.getProjectId());
        List<TogetherDaoTreasuryTransactionVo> transactionList = transactionPage.getRecords();

        // 转化为long类型时间戳
        for (TogetherDaoTreasuryTransactionVo transactionVo : transactionList) {
            transactionVo.setPayCurrencyType(dao.getPayCurrencyType());
            transactionVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
            transactionVo.setInputTokenDecimals(dao.getInputTokenDecimals());
            transactionVo.setDaoSymbol(dao.getDaoSymbol());
            transactionVo.setErc20Token(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
            transactionVo.setCreateTime(transactionVo.getCreateTimeStamp().getTime());
        }

        result.setDataList(transactionList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoIdParam.getPageNo());
        page.setPageSize(daoIdParam.getPageSize());
        page.setCount(transactionPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 1.6 国库页面 聚合dao下的dao列表信息
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/treasury/list")
    public ResultList<TreasuryTogetherDaoListVo> togetherTreasuryDaoList(@RequestBody DaoIdParam daoIdParam,
                                                                         HttpServletRequest request) {
        ResultList<TreasuryTogetherDaoListVo> result = new ResultList<>();
        if (daoIdParam == null || StringUtils.isBlank(daoIdParam.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdParam.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<Dao> iPage = new Page<>(daoIdParam.getPageNo(), daoIdParam.getPageSize());

        Page<Dao> daoListPage = daoService.selectByTogetherDaoIdPage(iPage, dao.getId() + "");

        List<Dao> daoList = daoListPage.getRecords();
        List<TreasuryTogetherDaoListVo> togetherDaoListVoList = daoList.stream().map(this::transferTreasuryTogetherDaoListVo).collect(Collectors.toList());
        result.setDataList(togetherDaoListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoIdParam.getPageNo());
        page.setPageSize(daoIdParam.getPageSize());
        page.setCount(daoListPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * 1.4.2 聚合dao下的dao列表信息
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/togetherDao/list")
    public ResultList<TogetherDaoListVo> togetherDaoList(@RequestBody DaoIdReqVo daoIdReqVo,
                                                         HttpServletRequest request) {
        log.info("接口开始时间:" + DateUtil.getCurrentTimestamp());
        ResultList<TogetherDaoListVo> result = new ResultList<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        log.info("寻找到所有的sub dao开始时间:" + DateUtil.getCurrentTimestamp());
        List<Dao> daoList = daoService.selectByTogetherDaoId(dao.getId() + "");
        daoList = daoList.stream().filter(v -> DaoStatusEnum.STARTED.getStatus().equals(v.getDaoStatus())).collect(Collectors.toList());

        log.info("转换为TogetherDaoListVo的开始时间:" + DateUtil.getCurrentTimestamp());
        // List<TogetherDaoListVo> togetherDaoListVoList = daoList.stream().sorted(Comparator.comparing(Dao::getDaoName)).map(this::transferTogetherDaoListVo).collect(Collectors.toList());
        List<TogetherDaoListVo> togetherDaoListVoList = daoList.stream().sorted(Comparator.comparing(Dao::getDaoName))
                .map(v -> TogetherDaoListVo.transferTogetherDaoListVo(v, userAddress))
                .collect(Collectors.toList());

        result.setDataList(togetherDaoListVoList);

        log.info("接口结束时间:" + DateUtil.getCurrentTimestamp());
        return result;

    }

    /**
     * 1.12 修改接口 dao详情页
     */
    @PostMapping(value = "/detail/v2")
    public Result<DaoDetailV2Vo> daoDetailV2(@RequestBody(required = false) DaoIdReqVo daoIdReqVo,
                                             HttpServletRequest request) {

        Result<DaoDetailV2Vo> result = new Result<>();
        log.info("[daoDetailV2] daoIdReqVo:{}", JacksonUtil.obj2json(daoIdReqVo));

        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());

        DaoDetailV2Vo daoDetailVo = DaoDetailV2Vo.transfer(dao, daoDrbStatistics);
        if (dao.getTogetherDaoId() != null) {
            Dao togetherDao = daoService.daoDetailByDaoId(dao.getTogetherDaoId());
            if (togetherDao != null) {
                daoDetailVo.setMainDaoId(togetherDao.getId());
                daoDetailVo.setMainDaoName(togetherDao.getDaoName());
                daoDetailVo.setMainDaoProjectId(togetherDao.getProjectId());

                // 1.12加入maker信息
                daoDetailVo.setTogetherDaoMakerVo(transferTogetherDaoMakerVo(dao));
            }
        }

        Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
        daoDetailVo.setMintWindow(Long.valueOf(daoDrbStatisticsPage.getTotal()).intValue());

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(userAddress)) {
            if (dao.getOwnerAddress().equalsIgnoreCase(userAddress)) {
                daoDetailVo.setModifiable(true);
            }

            Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(),
                    dao.getId() + "", userAddress);
            if (favorites != null) {
                daoDetailVo.setFavorited(true);
            }
        }
        if (StringUtils.isNotBlank(dao.getExistDaoId())) {
            Dao existDao = daoService.daoDetailByProjectId(dao.getExistDaoId());
            if (existDao != null) {
//                daoDetailVo.setMainDaoId(existDao.getId());
//                daoDetailVo.setMainDaoName(existDao.getDaoName());
//                daoDetailVo.setMainDaoProjectId(existDao.getProjectId());
            }
            if (existDao != null && existDao.getOwnerAddress().equals(userAddress)) {
                daoDetailVo.setIsMainDaoCreator(true);
            }
        } else {
            daoDetailVo.setIsMainDaoCreator(daoDetailVo.getModifiable());
        }

        daoDetailVo.setBasicInformation(commonService.getBasicInformationVo(dao));
        daoDetailVo.setMintWindowInfoVo(commonService.getMintWindowInfoVo(dao));
        daoDetailVo.setModeStatusVo(ModeStatusVo.transfer(dao));
        List<Dao> togetherDaoList = daoService.selectByTogetherDaoId(dao.getTogetherDaoId() + "");
        daoDetailVo.setTotalDaoNumber(togetherDaoList.size());

        // 根据区块计算百分比：
        /*
         * 分子 = 现在的区块数 - dao开始的区块 -（第几个周期*每个周期需要的区块数）
         * 分母 = 每个周期需要的区块数
         *
         * “每个周期需要的区块数”为 预估数量
         * 因为每个周期的区块数都 * 1e28，所以，其他的也都 *1e28
         * 每个周期需要的区块数 =
         * */
        Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
        if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("network anomaly！ please try again later!");
            return result;
        }

        if (DaoStatusEnum.STARTED.getStatus().equals(dao.getDaoStatus()) && Integer.parseInt(dao.getCurrentRound()) > 0) {
            //如果dao重新开始的话这样计算是有问题的 需要看合约有没有查询dao下一个drb的开始区块的方法
            String startBlock = CommonUtil.tenToHex(Integer.parseInt(dao.getDaoStartBlock()));
            Double blockNoTime = null;
            if (StringUtils.isNotBlank(startBlock)) {
                blockNoTime = commonService.blockNoStartTime(CommonUtil.addHexPrefixIfNotExist(startBlock));
            }
            log.info("[blockNoTime]blockNoTime:{}", blockNoTime);
            if (blockNoTime != null) {
                long nextMintWindowStartTime = new BigDecimal(String.valueOf(blockNoTime))
                        .add(
                                new BigDecimal(dao.getCurrentRound())
                                        .add(BigDecimal.ONE)
                                        .subtract(new BigDecimal(String.valueOf(dao.getLastModifyRound())))
                                        .multiply(new BigDecimal(dao.getDuration())
                                                .divide(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber), 18, RoundingMode.HALF_UP)
                                                .multiply(new BigDecimal("60")).multiply(new BigDecimal("60"))
                                        )
                        ).longValue();

                // 重新计算
//                BigDecimal durationNum = new BigDecimal(dao.getCurrentRound()).add(BigDecimal.ONE).subtract(new BigDecimal(String.valueOf(dao.getLastModifyRound()))); // 周期+1-重新开始 的数量
//                BigDecimal durationBlockTime = new BigDecimal(dao.getDuration())
//                        .divide(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber), 18, RoundingMode.HALF_UP)
//                        .multiply(new BigDecimal("60")).multiply(new BigDecimal("60")); // 每个周期大概需要多少‘块’的时间
//                BigDecimal durationBlockTime1 = durationNum.multiply(durationBlockTime);
//                nextMintWindowStartTime = new BigDecimal(String.valueOf(blockNoTime)).add(durationBlockTime1).longValue();

                long currentTime = System.currentTimeMillis() / 1000;
                log.info("[nextMintWindowStartTime]nextMintWindowStartTime:{} currentTime:{} ", nextMintWindowStartTime, currentTime);
                log.info("[currentTime]currentTime:{}", currentTime);
                if (nextMintWindowStartTime - currentTime > 0) {
                    daoDetailVo.setNextMintWindowStartTime(nextMintWindowStartTime - currentTime);
                }
                log.info("[currentTime]daoId:{} setNextMintWindowStartTime:{}", dao.getId(), nextMintWindowStartTime - currentTime);
            }

            // 百分比重新计算
            BigDecimal denominator = new BigDecimal(dao.getDuration()).divide(new BigDecimal("1e18"));  // 分母=每周期出块数量
            log.info("分母=每周期出块数量:" + denominator);
            BigDecimal blockNumber = new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数
            log.info("当前区块数:" + blockNumber);
            BigDecimal startBlockNumber = new BigDecimal(dao.getDaoStartBlock()); // 开始区块
            log.info("开始区块:" + startBlockNumber);
            BigDecimal currentRound = new BigDecimal(Integer.parseInt(dao.getCurrentRound()) - 1);   // 已经完成周期数
            log.info("当前周期数:" + currentRound);
            BigDecimal numThisCurrentRound = blockNumber.subtract(startBlockNumber).subtract(currentRound.multiply(denominator));

            if (dao.getDaoRestartBlock() != null) {
                log.info("重新开始的区块高度为:" + dao.getDaoRestartBlock());
                // 如果重新开始，当前周期内出的块应该加上余数..
                BigDecimal restartBlock = new BigDecimal(dao.getDaoRestartBlock());
                BigDecimal roundSub = restartBlock.subtract(startBlockNumber);
                log.info("重新开始的时间-开始的时间的差值:" + roundSub);
                BigDecimal[] resultDiv = roundSub.divideAndRemainder(denominator);
                log.info("时间差对每个周期的区块数相除的值:" + JacksonUtil.obj2json(resultDiv));
                BigDecimal blockRemainder = resultDiv[1];
                log.info("余数-这个周期合约中已经过了多少块:" + blockRemainder);
                numThisCurrentRound = numThisCurrentRound.subtract(blockRemainder);

                if (numThisCurrentRound.compareTo(denominator) > 0) {
                    log.info("重新开始的区块高度大于当前周期的区块数，重新计算:{}", numThisCurrentRound);
                    resultDiv = numThisCurrentRound.divideAndRemainder(denominator);
                    numThisCurrentRound = resultDiv[1];
                }
            }
            log.info("当前周期内已经出了多少块:" + numThisCurrentRound);

            BigDecimal numerator = denominator.subtract(numThisCurrentRound);
            log.info("分子：还有多少块到下个周期:" + numerator);

            // 计算结果
            numerator = numerator.compareTo(BigDecimal.ZERO) < 0 ? BigDecimal.ZERO : numerator;   // 如果计算出负数？
            BigDecimal proportion = numerator.divide(denominator, 2, RoundingMode.HALF_UP);
            DecimalFormat percentFormat = new DecimalFormat("#.##");
            percentFormat.setMultiplier(100);   // 默认*100 ,但是不加%
            String percentResult = percentFormat.format(proportion);
            daoDetailVo.setProportion(percentResult);

            // 倒计时时间 还有多少块到下个周期*每个出块的时间* 1000 转为毫秒
            // 每分钟的出块数量 = 每小时出块数量/60
            // 每秒的出块数量 = 60 / 每分钟的出块数量 = 16.5
            Long countdown = numerator.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).multiply(new BigDecimal("1000")).longValue();
            daoDetailVo.setCountdown(countdown);

            daoDetailVo.setTotalMintWindowTime(denominator.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).multiply(new BigDecimal("1000")).longValue());
        }


        // 1.13 需要加入当前node是否开启了分流比例
        List<DaoAllocationStrategy> daoAllocationStrategyList = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
        List<DaoAllocationVo> daoTokenAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
        List<DaoAllocationVo> daoEthAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());

        daoDetailVo.setIsAllocationStrategy(!daoTokenAllocationVos.isEmpty() || !daoEthAllocationVos.isEmpty());


//        if (StringUtils.isNotBlank(dao.getDuration())) {
//            daoDetailVo.setTotalMintWindowTime(new BigDecimal(dao.getDuration()).divide(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber), 18, RoundingMode.UP)
//                    .multiply(new BigDecimal("60")).multiply(new BigDecimal("60")).longValue());
//        }


        result.setData(daoDetailVo);
        return result;
    }

    // ==============================private separate============================================//

    /**
     * 构造查询时间
     *
     * @param daoAnalyticsReqDto 请求参数
     * @return DaoAnalyticsBo
     */
    private DaoAnalyticsBo buildFirstDaoAnalyticsBo(DaoAnalyticsReqVo daoAnalyticsReqDto) {
        LocalDate localDate = LocalDate.now();
        String endDate = localDate.format(FORMATTER);
        // 计算开始时间
        localDate = localDate.minusDays(daoAnalyticsReqDto.getDayTime() - 1);
        String firstStartDate = localDate.format(FORMATTER);
        DaoAnalyticsBo daoAnalyticsBo = new DaoAnalyticsBo();
        daoAnalyticsBo.setDaoId(daoAnalyticsReqDto.getDaoId());
        daoAnalyticsBo.setStartDate(firstStartDate);
        daoAnalyticsBo.setEndDate(endDate);
        daoAnalyticsBo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        return daoAnalyticsBo;
    }

    /**
     * @param daoAnalyticsReqDto 请求参数
     * @param daoAnalyticsBo     请求参数
     */
    private void buildSecondDaoAnalyticsBo(DaoAnalyticsReqVo daoAnalyticsReqDto, DaoAnalyticsBo daoAnalyticsBo) {
        LocalDate localDate = LocalDate.now();
        // 计算开始时间
        localDate = localDate.minusDays(daoAnalyticsReqDto.getDayTime());
        String firstStartDate = localDate.format(FORMATTER);
        localDate = localDate.minusDays(daoAnalyticsReqDto.getDayTime() - 1);
        String secondStartDate = localDate.format(FORMATTER);

        daoAnalyticsBo.setStartDate(secondStartDate);
        daoAnalyticsBo.setEndDate(firstStartDate);
    }

    private TogetherDaoListVo transferTogetherDaoListVo(Dao dao) {
        TogetherDaoListVo togetherDaoMakerVo = TogetherDaoListVo.transfer(dao);

        togetherDaoMakerVo.setBasicInformationVo(commonService.getBasicInformationVo(dao));
        togetherDaoMakerVo.setMintWindowInfoVo(commonService.getMintWindowInfoVo(dao));
        togetherDaoMakerVo.setModeStatusVo(ModeStatusVo.transfer(dao));

        //查询当前dao分配给其他dao的分配信息
        List<DaoAllocationStrategy> daoAllocationStrategyList = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
        List<DaoAllocationVo> daoTokenAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
        List<DaoAllocationVo> daoEthAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());

        togetherDaoMakerVo.setAllocationTokenToOtherDao(daoTokenAllocationVos);
        togetherDaoMakerVo.setAllocationEthToOtherDao(daoEthAllocationVos);

        //查询其他dao分配给当前dao的分配信息
        List<DaoAllocationStrategy> daoAllocationStrategyToProject = daoAllocationStrategyService.selectByProjectIdAndType(dao.getProjectId(), null);
        List<DaoAllocationVo> receivedTokenFromOther = daoAllocationStrategyToProject.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transferOriginProject).collect(Collectors.toList());
        List<DaoAllocationVo> receivedEthFromOther = daoAllocationStrategyToProject.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transferOriginProject).collect(Collectors.toList());


        togetherDaoMakerVo.setReceivedTokenFromOther(receivedTokenFromOther);
        togetherDaoMakerVo.setReceivedEthFromOther(receivedEthFromOther);

        return togetherDaoMakerVo;
    }

    private TreasuryTogetherDaoListVo transferTreasuryTogetherDaoListVo(Dao dao) {
        return TreasuryTogetherDaoListVo.transfer(dao);
    }

    private TogetherDaoMakerVo transferTogetherDaoMakerVo(Dao dao) {
        String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        return workTopupHarvestService.getTogetherDaoMakerVo(existDaoId);
    }


    public static void main(String[] args) {

//        String startBlock = CommonUtil.tenToHex(Integer.parseInt("10340666"));
//        System.out.println(startBlock);
//        long nextMintWindowStartTime = new BigDecimal(String.valueOf("1705027908"))
//                .add(new BigDecimal("0").add(BigDecimal.ONE).multiply(new BigDecimal("5222285092491838455168")
//                        .divide(new BigDecimal("217595212187159935632"), 18, RoundingMode.HALF_UP)
//                        .multiply(new BigDecimal("60")).multiply(new BigDecimal("60")))).longValue();
//        long currentTime = System.currentTimeMillis() / 1000;
//        System.out.println(nextMintWindowStartTime);
//        System.out.println(currentTime);
//        System.out.println(nextMintWindowStartTime - currentTime);


        BigDecimal denominator = new BigDecimal("217595212187159935632").divide(new BigDecimal("1e18"));  // 分母=每小时出块数量
        log.info("分母=每小时出块数量:" + denominator);

        BigDecimal blockNumber = new BigDecimal("10430803"); // 当前区块数
        log.info("当前区块数:" + blockNumber);
        BigDecimal startBlockNumber = new BigDecimal("10428911"); // 开始区块
        log.info("开始区块:" + startBlockNumber);
        log.info("当前-开始：" + blockNumber.subtract(startBlockNumber));

        BigDecimal currentRound = new BigDecimal(9 - 1);   // 当前周期数
        log.info("当前周期数:" + currentRound);
        log.info("已经计算完成的区块数：" + currentRound.multiply(denominator));

        BigDecimal numThisCurrentRound = blockNumber.subtract(startBlockNumber).subtract(currentRound.multiply(denominator));
        log.info("当前周期内已经出了多少块:" + numThisCurrentRound);
        BigDecimal numerator = denominator.subtract(numThisCurrentRound);
        log.info("分子：还有多少块到下个周期:" + numerator);

        BigDecimal proportion = numerator.divide(denominator, 2, RoundingMode.HALF_UP);
        DecimalFormat percentFormat = new DecimalFormat("#.##");
        percentFormat.setMultiplier(100);
        String percentResult = percentFormat.format(proportion);
        log.info("percentResult:" + percentResult);


        // 倒计时时间 还有多少块到下个周期*12(每个块的时间) *1000 转为毫秒
        Long countdown = numerator.multiply(new BigDecimal("16.5")).multiply(new BigDecimal("1000")).longValue();
        log.info("countdown:" + countdown);
        //countdown = 82653204L;

        long seconds = countdown / 1000; // 一共有多少秒
        long hours = seconds / 3600;    // 剩余小时数
        long minutes = (seconds % 3600) / 60;   // 剩余分钟数
        long remainingSeconds = seconds % 60;   // 剩余秒数

        System.out.println(hours + " hours " + minutes + " minutes " + remainingSeconds + " seconds");


        // 用未跑完的块数*12秒得到倒计时...


//        String object = "{\n" +
//                "    \"time\":\n" +
//                "    [\n" +
//                "        1702684800,\n" +
//                "        1702771200,\n" +
//                "        1702857600,\n" +
//                "        1702944000,\n" +
//                "        1703030400,\n" +
//                "        1703116800\n" +
//                "    ],\n" +
//                "    \"totalAmount\":\n" +
//                "    [\n" +
//                "        \"0.043991747265376785\",\n" +
//                "        \"0.043991747265376785\",\n" +
//                "        \"0.043991747265376785\",\n" +
//                "        \"0.043991747265376785\",\n" +
//                "        \"0.043991747265376785\",\n" +
//                "        \"0.043991747265376785\"\n" +
//                "    ],\n" +
//                "    \"incomes\":\n" +
//                "    [\n" +
//                "        \"0.3451456\",\n" +
//                "        \"0.3452456\",\n" +
//                "        \"0.34534564\",\n" +
//                "        \"0.4544564\",\n" +
//                "        \"0.3455656345\",\n" +
//                "        \"0.5463534\"\n" +
//                "    ],\n" +
//                "    \"costs\":\n" +
//                "    [\n" +
//                "        \"0.153452\",\n" +
//                "        \"0.452345345\",\n" +
//                "        \"0.453534534\",\n" +
//                "        \"0.4534435345\",\n" +
//                "        \"0.3534534\",\n" +
//                "        \"0.34536\"\n" +
//                "    ],\n" +
//                "    \"changes\":\n" +
//                "    [\n" +
//                "        \"0.1345\",\n" +
//                "        \"0.2354\",\n" +
//                "        \"0.5345343\",\n" +
//                "        \"0.354\",\n" +
//                "        \"0.53455\",\n" +
//                "        \"0.345346\"\n" +
//                "    ],\n" +
//                "    \"maxTotalAmount\": \"0.043991747265376785\",\n" +
//                "    \"maxIncomes\": \"0.5463534\",\n" +
//                "    \"maxCosts\": \"0.453534534\",\n" +
//                "    \"maxChanges\": \"0.53455\"\n" +
//                "}";
//        AssetPoolEthResVo assetPoolEthResVo = JacksonUtil.json2pojo(object, AssetPoolEthResVo.class);
//        System.out.println(JacksonUtil.obj2json(assetPoolEthResVo));

//        Timestamp today = DateUtil.getBeginOfToday();
//        long sevenDayBeginHour = DateUtil.getTimestampAfterDay(today, -6).getTime() / 1000;
//        System.out.println(sevenDayBeginHour);
    }

}
