package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.entity.Dao;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.TrueOrFalseEnum;

import java.math.BigDecimal;


/**
 * @description: Basic Information
 * @author: xiangbin
 * @create: 2023-12-27 14:15
 **/
@Slf4j
@Data
public class ModeStatusVo {

    /**
     * 1.4 乐透模式开关
     * <p>
     * 注意这里需要单独处理死了的DAO和开启无限模式的DAO，无限模式永远等于1，死了的DAO永远等于0
     */
    private Boolean lotteryMode = false;

    /**
     * 1.4 展示已经乐透的累积周期
     */
    private Integer lotteryDuration = 0;

    /**
     * 1.4 dao token模式开关
     */
    private Boolean daoToken = false;

    /**
     * 1.4 top-up模式的开关
     */
    private Boolean topupMode = false;

    /**
     * 1.4 全局统一一口价模式的开关
     */
    private Boolean unifiedPriceMode = false;

    /**
     * 1.4 全局统一一口价
     */
    private String unifiedPrice;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    /**
     * 1.4 策略的设置
     */
    private Boolean speicialStrategy = false;

    /**
     * 1.4 是否开启了无限模式
     */
    private Boolean infiniteMode = false;


    public static ModeStatusVo transfer(Dao dao) {

        ModeStatusVo modeStatusVo = new ModeStatusVo();
        modeStatusVo.setLotteryMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode()));
        int currentRound = dao.getCurrentRound() == null ? 0 : Integer.parseInt(dao.getCurrentRound());
        int lastActiveRound = dao.getLastActiveRound() == null ? 0 : dao.getLastActiveRound();
        log.info("[ModeStatusVo] currentRound:{} lastActiveRound:{} ", currentRound, lastActiveRound);
        if (modeStatusVo.getLotteryMode()) {
            modeStatusVo.setLotteryDuration(currentRound - lastActiveRound);
        }
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode())) {
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getInfiniteMode()) && DaoStatusEnum.STARTED.getStatus().equals(dao.getDaoStatus())) {
                modeStatusVo.setLotteryDuration(1);
            }
            if (DaoStatusEnum.FINISHED.getStatus().equals(dao.getDaoStatus())) {
                modeStatusVo.setLotteryDuration(0);
            }
        }
        modeStatusVo.setDaoToken(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));
        modeStatusVo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));
        modeStatusVo.setUnifiedPriceMode(dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0);
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            modeStatusVo.setUnifiedPrice(dao.getGlobalDaoPrice().stripTrailingZeros().toPlainString());
            modeStatusVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));
        }
        modeStatusVo.setSpeicialStrategy((dao.getCanvasCreatedBlacklist() + dao.getCanvasCreatedWhitelist() + dao.getCanvasCreatedWhitelistNft() + dao.getMinterWorksBlacklist()
                + dao.getMinterWorksWhitelist() + dao.getMinterWorksWhitelistNft() + dao.getMintCap() + dao.getErc721MintCap() + dao.getErc721MintCapId() + dao.getGlobalMintCap()) > 0);
        modeStatusVo.setInfiniteMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getInfiniteMode()));

        return modeStatusVo;

    }


}
