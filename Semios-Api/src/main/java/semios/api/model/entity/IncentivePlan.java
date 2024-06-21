package semios.api.model.entity;

import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.IdType;

import java.time.LocalDate;

import com.baomidou.mybatisplus.annotation.TableId;

import java.time.LocalDateTime;

import com.baomidou.mybatisplus.annotation.TableField;

import java.io.Serializable;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;
import semios.api.model.enums.Plan.PlanRewardEnum;
import semios.api.model.enums.Plan.PlanStatusEnum;
import semios.api.model.enums.Plan.PlanTypeEnum;

/**
 * <p>
 * 激励计划表
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("incentive_plan")
public class IncentivePlan implements Serializable {

    private static final long serialVersionUID = 1L;

    // id
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    // 聚合dao id
    private Integer daoId;

    // 聚合dao project id
    private String projectId;

    // 合约生成的planId
    private String planCode;

    // 合约生成的plan Number序号
    private Integer planNumber;

    // 记录的交易hash
    private String transactionHash;

    // 当前周期
    private Integer currentRound;

    // plan名称
    private String planName;

    // logo url
    private String planLogoUrl;

    /**
     * 计算激励贡献度的类型 1-input token 2-output token
     *
     * @see PlanTypeEnum
     */
    private Integer incentiveType;

    // input token symbol
    private String inputTokenSymbol;

    // output token symbol
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

    // 开始时间
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDate startDate;

    // 开始区块高度
    private String startBlock;

    // 奖励来源 1-国库 2-钱包
    private Integer amountSource;

    // 计划激励金额
    private BigDecimal incentiveAmount;

    // 剩余token数量
    private BigDecimal remainingToken;

    // plan的周期数量
    private Integer planBlockWindow;

    // 每个周期的周期持续时间，单位hours
    private Integer duration;

    /**
     * 计划状态  1-未开始 2-已开始 3-已结束
     *
     * @see PlanStatusEnum
     */
    private Integer incentiveStatus;

    // plan uri
    private String planUri;

    // plan create user address
    private String createBy;

    // 创建时间
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;


    // 区块高度
    @TableField(exist = false)
    private Integer erc20Decimal;

    // 区块高度
    @TableField(exist = false)
    private BigDecimal blockNumber;

    // token holders
    @TableField(exist = false)
    private Integer topupHolders;

    // input token amount
    @TableField(exist = false)
    private BigDecimal onChainEthBalance;

    // output token amount
    @TableField(exist = false)
    private BigDecimal onChainTokenBalance;

}
