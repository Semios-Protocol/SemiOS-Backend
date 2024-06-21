package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.service.ICanvasDrbStatisticsService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.RoundingMode;

/**
 * @description: dao income
 * @author: xiangbin
 * @create: 2022-08-08 11:20
 **/
@Data
public class UserCanvasIncomeVo {

    /**
     * canvas的canvasId 领取canvas代币用
     */
    private String canvasId;

    /**
     * dao名称
     */
    private String canvasName;

    /**
     * dao编号
     */
    private String daoNumber;

    /**
     * canvas编号
     */
    private String canvasNumber;

    /**
     * 用户收到的DAO的token
     */
    private String daoReward = "0";

    /**
     * 当前token总收入
     */
    private String balance = "0";

    /**
     * 当前dao可领取token数量
     */
    private String available = "0";

    /**
     * 版税收入
     */
    private String creatorFee = "0";

    /**
     * 版税收入余额
     */
    private String feeBalance = "0";

    /**
     * 铸造收入
     */
    private String mintRevenue = "0";

    /**
     * 铸造收入余额
     */
    private String mintBalance = "0";

    public static UserCanvasIncomeVo transfer(Canvas canvas) {
        UserCanvasIncomeVo userCanvasIncomeVo = new UserCanvasIncomeVo();
        userCanvasIncomeVo.setCanvasName(canvas.getCanvasName());
        userCanvasIncomeVo.setDaoNumber(canvas.getDaoNumber() + "");
        userCanvasIncomeVo.setCanvasNumber(canvas.getCanvasNumber() + "");
        userCanvasIncomeVo.setDaoReward(canvas.getUnclaimedToken().add(canvas.getReceivedToken()) + "");
        userCanvasIncomeVo.setBalance(canvas.getReceivedToken() + "");
        userCanvasIncomeVo.setAvailable(canvas.getUnclaimedToken() + "");
        userCanvasIncomeVo.setCanvasId(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
//        userCanvasIncomeVo.setMintRevenue();//固定展示ETH
        ICanvasDrbStatisticsService canvasDrbStatisticsService = SpringBeanUtil.getBean(ICanvasDrbStatisticsService.class);
        if (canvasDrbStatisticsService != null) {
            CanvasDrbStatistics canvasDrbStatistics = canvasDrbStatisticsService.selectLastedByCanvasId(canvas.getId());
            if (canvasDrbStatistics != null && canvasDrbStatistics.getTotalVol() != null) {
                userCanvasIncomeVo.setMintBalance(canvasDrbStatistics.getTotalVol().setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString());
                //临时传一样的，两个钱包余额取的字段不同
                userCanvasIncomeVo.setMintRevenue(canvasDrbStatistics.getTotalVol().setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString());
            }
        }
        return userCanvasIncomeVo;
    }

    public void setDaoReward(String daoReward) {
        if (StringUtils.isNotBlank(daoReward) && !daoReward.equals("null")) {
            this.daoReward = daoReward;
        }
    }

    public void setBalance(String balance) {
        if (StringUtils.isNotBlank(balance) && !balance.equals("null")) {
            this.balance = balance;
        }
    }

    public void setAvailable(String available) {
        if (StringUtils.isNotBlank(available) && !available.equals("null")) {
            this.available = available;
        }
    }

    public void setCreatorFee(String creatorFee) {
        if (StringUtils.isNotBlank(creatorFee) && !creatorFee.equals("null")) {
            this.creatorFee = creatorFee;
        }
    }

    public void setFeeBalance(String feeBalance) {
        if (StringUtils.isNotBlank(feeBalance) && !feeBalance.equals("null")) {
            this.feeBalance = feeBalance;
        }
    }

    public void setMintRevenue(String mintRevenue) {
        if (StringUtils.isNotBlank(mintRevenue) && !mintRevenue.equals("null")) {
            this.mintRevenue = mintRevenue;
        }
    }

    public void setMintBalance(String mintBalance) {
        if (StringUtils.isNotBlank(mintBalance) && !mintBalance.equals("null")) {
            this.mintBalance = mintBalance;
        }
    }
}
