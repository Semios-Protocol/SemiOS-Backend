package semios.api.controller;

import com.amazonaws.util.Md5Utils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.common.*;
import semios.api.model.dto.response.NewPlanUriDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.enums.Plan.PlanStatusEnum;
import semios.api.model.vo.req.DaoIdParam;
import semios.api.model.vo.req.Plan.CreatePlanParam;
import semios.api.model.vo.req.Plan.PlanIdReqVo;
import semios.api.model.vo.res.Plan.CreatePlanParamVo;
import semios.api.model.vo.res.Plan.PlanBasicInfoVo;
import semios.api.model.vo.res.Plan.PlanListVo;
import semios.api.model.vo.res.Plan.TogetherPlanVo;
import semios.api.model.vo.res.UserTopupBalanceVo;
import semios.api.service.IDaoService;
import semios.api.service.IIncentivePlanService;
import semios.api.service.IWorkTopupHarvestService;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.*;

import javax.servlet.http.HttpServletRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 激励计划表 前端控制器
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Slf4j
@RestController
@RequestMapping("/plan")
public class IncentivePlanController {

    @Autowired
    private S3Service s3Service;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IIncentivePlanService incentivePlanService;

    @Autowired
    private ISubscriptionService iSubscriptionService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;


    /**
     * 1.8 创建plan参数转换
     *
     * @param createPlanParam
     */
    @PostMapping(value = "/create/param")
    public Result<CreatePlanParamVo> createPlanInfo(@RequestBody CreatePlanParam createPlanParam,
                                                    HttpServletRequest request) {

        Result<CreatePlanParamVo> result = new Result<>();

        Dao dao = daoService.daoDetailByProjectId(createPlanParam.getProjectId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("can not find seed nodes");
            return result;
        }

        CreatePlanParamVo createPlanParamVo = new CreatePlanParamVo();
        createPlanParamVo.setErc20TokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        createPlanParamVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        // 因为 dao.getErc20TokenDecimals()在开启了20支付且decimal不等于18的时候才赋值
        // 当引入外部20未开启20支付时，decimal可能不对
        if (dao.getIsThirdpartyToken() == 1) {
            String decimal = commonService.erc20Decimals(dao.getErc20Token());
            createPlanParamVo.setErc20TokenDecimals(Integer.parseInt(decimal));
        } else {
            createPlanParamVo.setErc20TokenDecimals(18);
        }

        // 获取用户自己输入的 第三方的erc20 decimal
        if (StringUtils.isNotBlank(createPlanParam.getCustomTokenAddress())) {
            String decimal = commonService.erc20Decimals(createPlanParam.getCustomTokenAddress());
            createPlanParamVo.setCustomTokenAddress(createPlanParam.getCustomTokenAddress());
            createPlanParamVo.setCustomTokenDecimal(Integer.parseInt(decimal));
        }


        // createPlanParamVo.setRewardToken(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        createPlanParamVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        createPlanParamVo.setPayCurrencyType(dao.getPayCurrencyType());
        createPlanParamVo.setOwnerAddress(dao.getOwnerAddress());

        // 处理开启的区块高度
        int periodDays = 0;
        DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        if (StringUtils.isNotBlank(createPlanParam.getPlanStartDate())) {
            LocalDate dateParam = LocalDate.parse(createPlanParam.getPlanStartDate(), df);
            Period period = Period.between(LocalDate.now(), dateParam);
            periodDays = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
            periodDays = Math.max(periodDays, 0);
        }

        if (periodDays > 0) {
            LocalDate dateParam2 = LocalDate.parse(createPlanParam.getPlanStartDate(), df);
            Date yesterday = DateUtil.addDay(new Date(), -1);
            LocalDate dateParam = LocalDate.parse(sdf.format(yesterday), df);
            String yesterdayTime = DateUtil.getThisDayBeginTime(dateParam);
            String yesterdayBlockNo = ProtoDaoCommonUtil.timestampBlockNo(new BigDecimal(yesterdayTime).divide(new BigDecimal("1000"), 0, BigDecimal.ROUND_UP).toPlainString());
            log.info("[createBasicDao] yesterdayTime:{} yesterdayBlockNo:{} ", yesterdayTime, yesterdayBlockNo);
            if (StringUtils.isNotBlank(yesterdayBlockNo)) {

                Period period = Period.between(dateParam, dateParam2);
                int periods = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
                periods = Math.max(periods, 0);
                String startBlock = CommonUtil.calculateStartBlockNo(yesterdayBlockNo, periods).toString();
                startBlock = new BigDecimal(startBlock).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 0, RoundingMode.HALF_UP).toPlainString();
                log.info("[createPlanInfo] yesterdayBlockNo:{} periods:{} startBlock:{}", yesterdayBlockNo, periods, startBlock);
                createPlanParamVo.setStartBlock(startBlock);

            } else {
                //计算和12月11日00:00相差的天数
//                LocalDate dateParam1 = LocalDate.parse(ProtoDaoConstant.etherscanBlockDate, df);
//
//                Period period = Period.between(dateParam1, dateParam2);
//                int periods = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
//                periods = Math.max(periods, 0);
//                String startBlock = CommonUtil.calculateStartBlockHeight(periods).toString();
//                log.info("[createPlanInfo] dateParam1 periods:{} startBlock:{}", periods, startBlock);
//                createPlanParamVo.setStartBlock(startBlock);

                LocalDateTime midnight = LocalDateTime.of(dateParam2, LocalTime.MIDNIGHT);
                ZonedDateTime targetMidnight = midnight.atZone(ZoneId.systemDefault());
                ZonedDateTime now = ZonedDateTime.now(ZoneId.systemDefault());  // 获取时间?
                Duration duration = Duration.between(now, targetMidnight);
                log.info("选定时间到当前时间到小时数为:" + duration.toHours());

                Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
                if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
                    result.setResultCode(ResultDesc.ERROR.getResultCode());
                    result.setResultDesc("network anomaly！ please try again later!");
                    return result;
                }
                BigDecimal blockNumber = new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数
                BigDecimal block = blockNumber.add(new BigDecimal(CommonUtil.calculateStartBlockHeight(duration.toHours())));
                createPlanParamVo.setStartBlock(String.valueOf(block));
            }
        }


        // 处理每个duration的区块
        if (createPlanParam.getDuration() != null) {
            String duration = new BigDecimal(createPlanParam.getDuration()).multiply(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber)).divide(new BigDecimal("1e18"), 0, RoundingMode.HALF_UP).toString();
            // String duration = new BigInteger(String.valueOf(createPlanParam.getDuration())).multiply(new BigInteger(ProtoDaoConstant.etherscanBlockNumber)).toString();
            log.info("[createPlanInfo] duration:{}", duration);
            createPlanParamVo.setDurationBlock(duration);
        }


        String s3FileName;
        String planUriHash;
        try {
            // 处理uri
            NewPlanUriDto newPlanUriDto = NewPlanUriDto.transfer(createPlanParam);
            String sourceString = JacksonUtil.obj2json(newPlanUriDto);
            assert sourceString != null;
            byte[] sourceByte = sourceString.getBytes(StandardCharsets.UTF_8);
            log.info("[createPlanInfo] sourceByte:{}", sourceByte);

            long second = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            planUriHash = Md5Utils.md5AsBase64(sourceByte) + String.valueOf(second).substring(5);
            planUriHash = CommonUtil.replaceOperator(planUriHash);
            String fileName = planUriHash + ".json";
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);

            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(newPlanUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.planBucketName,
                    representaion);
            s3FileName = urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.planBucketName + "/" + fileName;
            log.info("[createPlanInfo] s3FileName:{}", s3FileName);
            createPlanParamVo.setPlanUri(s3FileName);
        } catch (Exception e) {
            log.error("[createPlanInfo] upload newPlanUriDto error createPlanParam:{} e:{}", JacksonUtil.obj2json(createPlanParam),
                    e.getMessage());
            result.setResultDesc("network error please try again later.");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }


        result.setData(createPlanParamVo);
        return result;
    }


    /**
     * 1.8 聚合dao的plan信息
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/together/tap")
    public Result<TogetherPlanVo> togetherDaoPlan(@RequestBody DaoIdParam daoIdParam,
                                                  HttpServletRequest request) {

        Result<TogetherPlanVo> result = new Result<>();
        Dao dao = daoService.getById(daoIdParam.getDaoId());
        if (dao == null || dao.getIsTogetherDao() != 1) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("can not find seed nodes");
            return result;
        }
        TogetherPlanVo togetherPlanVo = incentivePlanService.selectAllPlanByProjectIdGroup(dao.getProjectId());
        togetherPlanVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));

        // group by...
//        List<IncentivePlan> planList = incentivePlanService.selectAllPlanByProjectId(dao.getProjectId());
//        togetherPlanVo.setPlanTotal((long) planList.size());
//
//        Long notStarted = planList.stream().filter(plan -> PlanStatusEnum.NOT_STARTED.getBasicType().equals(plan.getIncentiveStatus())).count();
//        togetherPlanVo.setPlanNotStarted(notStarted);
//
//        Long planOngoing = planList.stream().filter(plan -> PlanStatusEnum.STARTED.getBasicType().equals(plan.getIncentiveStatus())).count();
//        togetherPlanVo.setPlanOngoing(planOngoing);
//
//        Long planEnd = planList.stream().filter(plan -> PlanStatusEnum.FINISHED.getBasicType().equals(plan.getIncentiveStatus())).count();
//        togetherPlanVo.setPlanEnd(planEnd);

        result.setData(togetherPlanVo);
        return result;
    }


    /**
     * 1.8 聚合页面plan卡片列表
     * （参数daoId为聚合dao的daoID）
     */
    @PostMapping(value = "/together/list")
    public ResultList<PlanListVo> togetherPlanList(@RequestBody DaoIdParam daoIdParam,
                                                   HttpServletRequest request) {
        // TODO 1.8.1 添加字段
        ResultList<PlanListVo> result = new ResultList<>();

        Dao dao = daoService.getById(daoIdParam.getDaoId());
        if (dao == null || dao.getIsTogetherDao() != 1) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("can not find seed nodes");
            return result;
        }

        Result<String> resultBlockNum = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
        if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("network anomaly！ please try again later!");
            return result;
        }
        BigDecimal blockNumber = new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数
        log.info("[togetherPlanList] ethGetBlockNumber blockNumber:{}", blockNumber);

        Integer decimal = Integer.parseInt(commonService.erc20Decimals(dao.getErc20Token()));

        UserTopupBalanceVo userTopupBalanceVo = workTopupHarvestService.selectSumOnChainTokenByProjectId(dao.getProjectId());
        Integer topupHolders = workTopupHarvestService.getTopupHoldersByProjectId(dao.getProjectId());


        Page<IncentivePlan> iPage = new Page<>(daoIdParam.getPageNo(), daoIdParam.getPageSize());
        Page<IncentivePlan> incentivePlanPage = incentivePlanService.selectAllPlanByProjectIdPage(iPage, dao.getProjectId());
        List<IncentivePlan> planList = incentivePlanPage.getRecords();

        List<PlanListVo> planListVos = new ArrayList<>();
        for (IncentivePlan plan : planList) {
            plan.setBlockNumber(blockNumber);
            plan.setOnChainEthBalance(userTopupBalanceVo.getOnChainEthBalance());
            plan.setOnChainTokenBalance(userTopupBalanceVo.getOnChainTokenBalance());
            plan.setTopupHolders(topupHolders);
            plan.setErc20Decimal(decimal);

            planListVos.add(PlanListVo.transferToPlanListVo(plan, dao));
        }

        result.setDataList(planListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoIdParam.getPageNo());
        page.setPageSize(daoIdParam.getPageSize());
        page.setCount(incentivePlanPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 1.8 plan卡片 basic info
     */
    @PostMapping(value = "/basic/info")
    public Result<PlanBasicInfoVo> planBaseInfo(@RequestBody PlanIdReqVo planIdReqVo,
                                                HttpServletRequest request) {
        // TODO 1.8.1 添加字段
        Result<PlanBasicInfoVo> result = new Result<>();
        IncentivePlan incentivePlan = incentivePlanService.getById(planIdReqVo.getPlanId());
        if (incentivePlan == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("can not find plan");
            return result;
        }

        Dao dao = daoService.daoDetailByProjectId(incentivePlan.getProjectId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("can not find seed nodes");
            return result;
        }
        Integer decimal = Integer.parseInt(commonService.erc20Decimals(dao.getErc20Token()));


        PlanBasicInfoVo planBasicInfoVo = new PlanBasicInfoVo();
        BeanUtil.copyProperties(incentivePlan, planBasicInfoVo);
        planBasicInfoVo.setPlanId(planIdReqVo.getPlanId());
        planBasicInfoVo.setPlanCode(CommonUtil.addHexPrefixIfNotExist(incentivePlan.getPlanCode()));

        planBasicInfoVo.setErc20TokenDecimals(decimal);
        planBasicInfoVo.setInputTokenSymbol(dao.getPayCurrencyType());
        planBasicInfoVo.setOutputTokenSymbol(dao.getDaoSymbol());

        int remainBlock = 0;    // 如果已经结束，剩余周期数=0
        if (incentivePlan.getIncentiveStatus().equals(PlanStatusEnum.STARTED.getBasicType())) {
            // 如果已经开始，剩余周期数+1
            remainBlock = incentivePlan.getPlanBlockWindow() - incentivePlan.getCurrentRound() + 1; // +1为包含当前周期
        } else if (incentivePlan.getIncentiveStatus().equals(PlanStatusEnum.NOT_STARTED.getBasicType())) {
            // 如果未开始，剩余周期数=周期数
            remainBlock = incentivePlan.getPlanBlockWindow();
        }
        planBasicInfoVo.setRemainBlock(Math.max(remainBlock, 0)); // 剩余区块数  不可以小于0

        planBasicInfoVo.setReleasedAmount(incentivePlan.getIncentiveAmount().subtract(incentivePlan.getRemainingToken())); // 已释放金额
        planBasicInfoVo.setStartDate(incentivePlan.getStartDate().atStartOfDay(ZoneOffset.UTC).toInstant().toEpochMilli()); // 开始时间

        planBasicInfoVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        planBasicInfoVo.setErc20TokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        result.setData(planBasicInfoVo);
        result.setResultCode(ResultDesc.SUCCESS.getResultCode());
        result.setResultDesc(ResultDesc.SUCCESS.getResultDesc());

        return result;
    }

}
