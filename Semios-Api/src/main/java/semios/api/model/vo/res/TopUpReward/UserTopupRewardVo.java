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
public class UserTopupRewardVo {

    /**
     * 聚合 daoId
     */
    private Integer daoId;

    /**
     * dao project Id
     */
    private String projectId;

    /**
     * 聚合 dao名称
     */
    private String daoName;

    /**
     * DAO的logo地址
     */
    private String daoLogoUrl;

    /**
     * dao output token symbol
     */
    private String daoSymbol;


    /**
     * dao intput token symbol
     */
    private String payCurrencyType;

    /**
     * dao erc20 token address
     */
    private String daoErc20Address;


    /**
     * 已经领取的数量
     */
    // private BigDecimal totalCollected;

    /**
     * 未领取的数量
     */
    private BigDecimal totalCollectable;


    /**
     * 未领取的detail信息
     */
    // private List<UserTopupRewardDetailVo> rewardDetailList;

    // @JsonIgnore
    // private String ownerAddress;


//    public static List<UserTopupRewardVo> transfer(List<UserTopupRewardVo> userTopupRewardDetailVoList){
//        ICollectRecordService collectRecordService = SpringBeanUtil.getBean(ICollectRecordService.class);
//        INftRewardAllocationService nftRewardAllocationService = SpringBeanUtil.getBean(INftRewardAllocationService.class);
//        if (collectRecordService==null || nftRewardAllocationService==null){
//            return new ArrayList<>();
//        }
//
//        for (UserTopupRewardVo userTopupRewardDetailVo : userTopupRewardDetailVoList) {
//            userTopupRewardDetailVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(userTopupRewardDetailVo.getProjectId()));
//            // 获取已经领取的数量
//            BigDecimal totalCollected = collectRecordService.getTotalCollectedByDaoId(userTopupRewardDetailVo.getDaoId(),userTopupRewardDetailVo.getOwnerAddress());
//
//            userTopupRewardDetailVo.setTotalCollected(totalCollected==null?BigDecimal.ZERO:totalCollected);
//            // 获取一个dao下的plan明细
//            List<UserTopupRewardDetailVo> rewardDetailList = nftRewardAllocationService.selectUserTopupRewardDetailVo(userTopupRewardDetailVo.getDaoId(),userTopupRewardDetailVo.getOwnerAddress());
//            userTopupRewardDetailVo.setRewardDetailList(rewardDetailList);
//        }
//
//
//        return userTopupRewardDetailVoList;
//    }

}
