package semios.api.model.vo.res.TopUpReward;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;

/**
 * 1.8 topup余额展示
 *
 * @description: topup balance
 * @author: xiangbin
 * @create: 2023-11-21 11:20
 **/
@Data
@Slf4j
public class UserTopupRewardDetailVo {

    /**
     * planId
     */
    private Integer planId;

    /**
     * planCode
     */
    private String planCode;

    /**
     * plan 编号
     */
    private Integer planNumber;


    // plan激励的token地址(input token,output token,custom token)
    private String rewardToken;

    // reward token 的symbol
    private String rewardTokenSymbol;

    /**
     * seed nodes下这个plan 已经领取 的数量，所有nft的owner为当前登陆用户的加和
     */
    private BigDecimal collectedAmount;

    /**
     * seed nodes下这个plan 可以领取  的数量，所有nft的owner为当前登陆用户的加和
     */
    private BigDecimal collectableAmount;

}
