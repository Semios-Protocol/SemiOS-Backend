package semios.api.model.vo.res.Plan;

import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Dao;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.enums.Plan.PlanRewardEnum;
import semios.api.model.enums.Plan.PlanStatusEnum;
import semios.api.model.enums.Plan.PlanTypeEnum;
import semios.api.utils.BeanUtil;
import semios.api.utils.CommonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.Instant;

@Data
public class PlanListVo {
    private static final Logger log = LoggerFactory.getLogger(PlanListVo.class);

    /**
     * plan表的主键ID
     *
     * @mock 1
     */
    private Integer planId;

    /**
     * seed nodes名称
     *
     * @mock zhy
     */
    private String daoName;

    /**
     * dao output token decimal
     *
     * @mock zhy
     */
    private Integer erc20TokenDecimals;

    /**
     * logo地址
     *
     * @mock http://...
     */
    private String planLogoUrl;

    /**
     * plan编号
     *
     * @mock 1
     */
    private Integer planNumber;

    /**
     * 合约生成的planCode
     *
     * @mock 1qaz2wsx3edc....
     */
    private String planCode;

    /**
     * plan的最大年化收益率
     *
     * @mock 1234.56
     */
    private String planAPR;

    /**
     * 计算激励贡献度的类型 1-input token 2-output token
     *
     * @mock 1
     */
    private Integer incentiveType;

    /**
     * input token symbol
     *
     * @mock 1
     */
    private String inputTokenSymbol;

    /**
     * output token symbol
     *
     * @mock 1
     */
    private String outputTokenSymbol;


    /**
     * plan激励的token方式 1-input 2-output 3-custom
     *
     * @see PlanRewardEnum
     */
    private Integer rewardType;

    // plan激励的token地址(input token,output token,custom token)
    private String rewardToken;

    // reward token 的symbol
    private String rewardTokenSymbol;

    // reward token 的 decimal
    private Integer rewardTokenDecimal;


    // plan current info
    /**
     * incentive plan的状态 1-未开始 2-已开始 3-已结束
     *
     * @mock 1
     */
    private Integer incentiveStatus;

    /**
     * 参与这个周期的人数
     *
     * @mock 1
     */
    private Integer topupHolders;


    /**
     * seed nodes下的top-up balance
     *
     * @mock 123
     */
    private BigDecimal topupBalance;

    /**
     * 这个周期出块多少钱
     *
     * @mock 123
     */
    private BigDecimal blockReward;

    /**
     * plan的block结束的时间(时间戳)
     *
     * @mock 123
     */
    private Long blockEndTime;

    // add token 相关
    /**
     * erc20 address
     */
    private String erc20TokenAddress;

    /**
     * input token address,如果是ETH为空
     */
    private String inputTokenAddress;

    /**
     * dao的owner address
     */
    private String ownerAddress;


    public static PlanListVo transferToPlanListVo(IncentivePlan plan, Dao dao) {
        PlanListVo planListVo = new PlanListVo();
        BeanUtil.copyProperties(plan, planListVo);
        planListVo.setPlanId(plan.getId());
        planListVo.setPlanCode(CommonUtil.addHexPrefixIfNotExist(plan.getPlanCode()));
        planListVo.setDaoName(dao.getDaoName());
        planListVo.setOwnerAddress(dao.getOwnerAddress());

        planListVo.setErc20TokenDecimals(plan.getErc20Decimal());
        planListVo.setInputTokenSymbol(dao.getPayCurrencyType());
        planListVo.setOutputTokenSymbol(dao.getDaoSymbol());
        planListVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        planListVo.setErc20TokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        planListVo.setPlanAPR(computedAPR(plan));   // 年华收益率只计算 挂帐的output token

        if (PlanStatusEnum.STARTED.getBasicType().equals(plan.getIncentiveStatus())) {
            // topupHolders 在一个seed node 下所有值都一样，不用重复从数据库中查询
            planListVo.setTopupHolders(plan.getTopupHolders());

            // topupBalance
            if (plan.getIncentiveType().equals(PlanTypeEnum.INPUT_TOKEN.getBasicType())) {
                planListVo.setTopupBalance(plan.getOnChainEthBalance());
            } else {
                planListVo.setTopupBalance(plan.getOnChainTokenBalance());
            }

            // blockReward = 剩余的token/总共还有多少个周期要发方奖励
            int blockWindow = plan.getPlanBlockWindow() - plan.getCurrentRound() + 1; // 剩余的周期数量 + 1 = 总共还有多少个周期要发方奖励
            planListVo.setBlockReward(plan.getRemainingToken().divide(new BigDecimal(blockWindow), 4, RoundingMode.HALF_UP));

            // blockEndTime
            planListVo.setBlockEndTime(computedBlockEndTime(plan));
        }

        return planListVo;
    }

    private static String computedAPR(IncentivePlan plan) {
        // 该seed nodes下所有的top-up余额的output token加和为X
        // 该plan的 总共奖励 设为Y
        // Plan的Duration*Block的乘积设为Z 单位是小时
        // （Y/Z）*24*365 转化为年华收益率  设为Q
        // APR = Q / X
        BigDecimal onChainTopupBalance;
        if (plan.getRewardType().equals(PlanRewardEnum.OUTPUT_TOKEN.getBasicType())) {
            if (plan.getOnChainTokenBalance().compareTo(BigDecimal.ZERO) <= 0) {
                return "0.00";
            }
            onChainTopupBalance = plan.getOnChainTokenBalance(); // X
        } else if (plan.getRewardType().equals(PlanRewardEnum.INPUT_TOKEN.getBasicType())) {
            if (plan.getOnChainEthBalance().compareTo(BigDecimal.ZERO) <= 0) {
                return "0.00";
            }
            onChainTopupBalance = plan.getOnChainEthBalance(); // X
        } else {
            // 如果是第三方引入的erc20 token，不用计算，直接返回
            return "0.00";
        }

        if (plan.getIncentiveAmount().compareTo(BigDecimal.ZERO) <= 0 || plan.getDuration() * plan.getPlanBlockWindow() == 0) {
            return "0.00";
        }

        log.info("onChainTopupBalance:{}", onChainTopupBalance);

        BigDecimal incentiveAmount = plan.getIncentiveAmount(); // Y
        log.info("incentiveAmount:{}", incentiveAmount);

        int totalHours = plan.getDuration() * plan.getPlanBlockWindow();   // Z
        log.info("totalHour:{}", totalHours);

        // 年发放token的数量
        BigDecimal tokenRewardYear = incentiveAmount.divide(new BigDecimal(totalHours), 18, RoundingMode.HALF_UP).multiply(new BigDecimal(24 * 365)); // Q
        BigDecimal proportion = tokenRewardYear.divide(onChainTopupBalance, 2, RoundingMode.HALF_UP);
        DecimalFormat percentFormat = new DecimalFormat("#.##");
        percentFormat.setMultiplier(100);   // 默认*100 ,但是不加%
        String percentResult = percentFormat.format(proportion);
        log.info("percentResult:{}", percentResult);

        return percentResult;
    }

    private static Long computedBlockEndTime(IncentivePlan plan) {
        // 当前周期到什么时间结束
        // 当前周期已经执行了多少块 = 现在的区块数 - plan开始的区块 -（已经完成的周期*每个周期需要的区块数）
        // 当前周期还有块结束 = 每个周期的块 - 当前周期已经执行了多少块
        BigDecimal blockNumber = plan.getBlockNumber();
        log.info("当前区块数:{}", blockNumber);

        BigDecimal startBlockNumber = new BigDecimal(plan.getStartBlock()); // 开始区块
        log.info("开始区块:{}", startBlockNumber);

        BigDecimal currentRound = new BigDecimal(plan.getCurrentRound() - 1);   // 已经完成周期数
        log.info("当前周期数:{}", currentRound);

        // 每个周期需要的区块数
        BigDecimal roundBlock = new BigDecimal(plan.getDuration()).multiply(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber)).divide(new BigDecimal("1e18"), 18, RoundingMode.HALF_UP);
        log.info("每个周期需要的区块数:{}", roundBlock);

        // 当前周期已经执行了多少块
        BigDecimal numThisCurrentRound = blockNumber.subtract(startBlockNumber).subtract(currentRound.multiply(roundBlock));
        log.info("当前周期已经执行了多少块:{}", numThisCurrentRound);

        // 当前周期还有多少块结束
        BigDecimal numNextRound = roundBlock.subtract(numThisCurrentRound);
        log.info("当前周期还有多少块结束:{}", numNextRound);

        // 每秒的出的块 对还有多少块转换为时间
        long countdown = numNextRound.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).longValue();
        log.info("剩余块数转换为时间多少秒:{}", countdown);

        // 如果计算出超过duration的时间，按照duration的时间来
        if (countdown > plan.getDuration() * 3600) {
            countdown = plan.getDuration() * 3600;
            log.info("剩余块数转换为时间超过了duration的时间，转换为duration的秒数:{}", countdown);
        }

        Instant now = Instant.now();
        Instant future = now.plusSeconds(countdown);
        // 获取新时间并转换为时间戳
        return future.getEpochSecond();
    }

}
