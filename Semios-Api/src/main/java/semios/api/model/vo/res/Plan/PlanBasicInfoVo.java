package semios.api.model.vo.res.Plan;

import lombok.Data;
import semios.api.model.enums.Plan.PlanRewardEnum;

import java.math.BigDecimal;

@Data
public class PlanBasicInfoVo {
    /**
     * plan表的主键ID
     *
     * @mock 1
     */
    private Integer planId;

    /**
     * 合约生成的planCode
     *
     * @mock 1qaz2wsx3edc....
     */
    private String planCode;

    /**
     * 计算激励贡献度的类型 1-input token 2-output token
     *
     * @mock 1
     */
    private Integer incentiveType;

    /**
     * dao output token decimal
     *
     * @mock zhy
     */
    private Integer erc20TokenDecimals;

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


    /**
     * Remaining Block 剩余周期数量
     *
     * @mock 10
     */
    private Integer remainBlock;

    /**
     * 这个Nodes的Block Duration为多久
     *
     * @mock 123
     */
    private Integer duration;

    /**
     * 计划激励金额
     *
     * @mock 12345678
     */
    private BigDecimal incentiveAmount;

    /**
     * 每个Block释放出来的Token加和,已经释放多少token
     *
     * @mock 12345678
     */
    private BigDecimal releasedAmount;

    /**
     * plan 开始时间
     *
     * @mock 12345678
     */
    private Long startDate;


    // 合约需要用到
    /**
     * erc20 address
     */
    private String erc20TokenAddress;

    /**
     * input token address,如果是ETH为空
     */
    private String inputTokenAddress;

}
