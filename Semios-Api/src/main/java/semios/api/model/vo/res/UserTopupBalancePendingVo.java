package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.entity.Dao;
import semios.api.model.enums.DaoTogetherTypeEnum;
import semios.api.model.vo.req.DaoProjectVo;
import semios.api.service.IDaoService;
import semios.api.service.IWorkTopupHarvestService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

/**
 * 1.3 topup余额展示
 *
 * @description: topup balance
 * @author: xiangbin
 * @create: 2023-11-21 11:20
 **/
@Data
@Slf4j
public class UserTopupBalancePendingVo {

    /**
     * seed nodes daoId
     */
    private Integer daoId;

    /**
     * seed nodes projectId
     */
    private String projectId;

    /**
     * seed nodes 名称
     */
    private String daoName;

    /**
     * DAO的logo地址
     */
    private String daoLogoUrl;

    /**
     * dao描述
     */
    private String daoDescription;

    /**
     * out put token symbol
     */
    private String daoSymbol;

    /**
     * input token symbol
     */
    private String payCurrencyType;

    /**
     * dao编号,聚合dao没有dao number
     *
     * @ignore
     */
    private String daoNumber;


    /**
     * 未领取的detail信息
     */
    // private List<UserTopupBalancePendingDetailVo> pendingList;
    public static UserTopupBalancePendingVo transfer(DaoProjectVo daoProjectVo) {
        UserTopupBalancePendingVo userTopupBalancePendingVo = new UserTopupBalancePendingVo();
        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        IWorkTopupHarvestService workTopupHarvestService = SpringBeanUtil.getBean(IWorkTopupHarvestService.class);
        if (daoService == null || workTopupHarvestService == null) {
            return new UserTopupBalancePendingVo();
        }

        Dao dao = daoService.getDaoByProjectId(daoProjectVo.getProjectId(), DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus()); // 获取聚合dao信息
        // 聚合dao的信息
        userTopupBalancePendingVo.setDaoId(dao.getId());
        userTopupBalancePendingVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        userTopupBalancePendingVo.setDaoName(dao.getDaoName());
        userTopupBalancePendingVo.setDaoLogoUrl(dao.getDaoLogoUrl());
        userTopupBalancePendingVo.setDaoDescription(dao.getDaoDescription());
        userTopupBalancePendingVo.setDaoSymbol(dao.getDaoSymbol());
        userTopupBalancePendingVo.setPayCurrencyType(dao.getPayCurrencyType());

//        List<UserTopupBalancePendingDetailVo> userTopupBalancePendingDetailVos = workTopupHarvestService.selectTopUpPendingSeeMore(daoProjectVo.getUserAddress(), daoProjectVo.getProjectId());
//        for (UserTopupBalancePendingDetailVo userTopupBalancePendingDetailVo : userTopupBalancePendingDetailVos){
//            userTopupBalancePendingDetailVo.setOperationTime(userTopupBalancePendingDetailVo.getCreateTimestamp().getTime());
//
//            // minted dao 可能是sub dao
//            Dao mintDao = daoService.getById(userTopupBalancePendingDetailVo.getMintedDaoId());
//            userTopupBalancePendingDetailVo.setEndBlockTime(computedEndBlockTime(mintDao,blockNumber));
//        }
//        userTopupBalancePendingVo.setPendingList(userTopupBalancePendingDetailVos);


        return userTopupBalancePendingVo;
    }

//    private static Long computedEndBlockTime(Dao dao,BigDecimal blockNumber){
//        if (dao==null){
//            return null;
//        }
//        BigDecimal denominator = new BigDecimal(dao.getDuration()).divide(new BigDecimal("1e18"));  // 分母=每周期出块数量
//        log.info("分母=每周期出块数量:"+denominator);
//        log.info("当前区块数:"+blockNumber);
//        BigDecimal startBlockNumber = new BigDecimal(dao.getDaoStartBlock()); // 开始区块
//        log.info("开始区块:"+startBlockNumber);
//        BigDecimal currentRound = new BigDecimal(Integer.parseInt(dao.getCurrentRound())-1);   // 已经完成周期数
//        log.info("当前周期数:"+currentRound);
//        BigDecimal numThisCurrentRound = blockNumber.subtract(startBlockNumber).subtract(currentRound.multiply(denominator));
//
//        if (dao.getDaoRestartBlock()!=null){
//            log.info("重新开始的区块高度为:"+dao.getDaoRestartBlock());
//            BigDecimal restartBlock = new BigDecimal(dao.getDaoRestartBlock());
//            BigDecimal roundSub = restartBlock.subtract(startBlockNumber);
//            log.info("重新开始的时间-开始的时间的差值:"+roundSub);
//            BigDecimal[] resultDiv = roundSub.divideAndRemainder(denominator);
//            log.info("时间差对每个周期的区块数相除的值:"+ JacksonUtil.obj2json(resultDiv));
//            BigDecimal blockRemainder = resultDiv[1];
//            numThisCurrentRound = numThisCurrentRound.subtract(blockRemainder);
//        }
//        log.info("当前周期内已经出了多少块:"+numThisCurrentRound);
//
//        BigDecimal numerator = denominator.subtract(numThisCurrentRound);
//        log.info("分子：还有多少块到下个周期:"+numerator);
//        return numerator.multiply(new BigDecimal(ProtoDaoConstant.BLOCK_SECONDS)).multiply(new BigDecimal("1000")).longValue();
//    }

}
