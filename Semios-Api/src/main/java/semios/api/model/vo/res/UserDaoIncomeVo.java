package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.utils.CommonUtil;

/**
 * @description: dao income
 * @author: xiangbin
 * @create: 2022-08-08 11:20
 **/
@Data
public class UserDaoIncomeVo {

    /**
     * dao的projectId
     */
    private String projectId;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * dao编号
     */
    private String daoNumber;

    /**
     * 用户收到的DAO的token
     */
    private String creatorReward = "0";

    /**
     * token余额
     */
    private String balance = "0";

    /**
     * 当前可领取的token数量
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

    public static UserDaoIncomeVo transfer(Dao dao) {

        UserDaoIncomeVo userDaoIncomeVo = new UserDaoIncomeVo();
        userDaoIncomeVo.setDaoName(dao.getDaoName());
        userDaoIncomeVo.setDaoNumber(dao.getDaoNumber() + "");
        userDaoIncomeVo.setCreatorReward(dao.getUnclaimedToken().add(dao.getReceivedToken()).stripTrailingZeros().toPlainString());
        userDaoIncomeVo.setBalance(dao.getReceivedToken() + "");
        userDaoIncomeVo.setAvailable(dao.getUnclaimedToken() + "");
        userDaoIncomeVo.setCreatorFee(dao.getDaoCreateFee() + "");
        userDaoIncomeVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        return userDaoIncomeVo;
    }

    public void setCreatorReward(String creatorReward) {
        if (StringUtils.isNotBlank(creatorReward) && !creatorReward.equals("null")) {
            this.creatorReward = creatorReward;
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
}
