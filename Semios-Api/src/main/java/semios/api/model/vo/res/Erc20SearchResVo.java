package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.entity.Dao;

import java.math.BigDecimal;

/**
 * @description:
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class Erc20SearchResVo {

    // 查dao的name、logo、Total Supply、已发放、burn、流通量、最大流通量（最后两个不给也行，dex这头自己算也可以）
    /**
     * erc20Address
     */
    private String erc20Address;

    /**
     * daoName
     */
    private String daoName;

    /**
     * dao logo
     */
    private String daoLogo;

    /**
     * Total Supply
     */
    private String totalSupply;

    /**
     * 已发放代币
     */
    private BigDecimal issued;

    /**
     * burn的数量
     */
    private BigDecimal brunAmount;

    /**
     * 版税二次交易收益
     */
    private BigDecimal royaltyFeeIncome;

    public static Erc20SearchResVo transfer(Dao dao) {
        Erc20SearchResVo erc20SearchResVo = new Erc20SearchResVo();
        erc20SearchResVo.setErc20Address(dao.getErc20Token());
        erc20SearchResVo.setDaoName(dao.getDaoName());
        erc20SearchResVo.setDaoLogo(dao.getDaoLogoUrl());
        erc20SearchResVo.setTotalSupply(dao.getErc20TotalSupply());
        erc20SearchResVo.setIssued(dao.getDaoReward());
        erc20SearchResVo.setBrunAmount(dao.getBurnAmount());
        erc20SearchResVo.setRoyaltyFeeIncome(dao.getRoyaltyFeeIncome());
        return erc20SearchResVo;
    }

}
