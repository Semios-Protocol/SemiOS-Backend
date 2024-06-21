package semios.dex.model.vo.res;

import lombok.Data;

import java.math.BigDecimal;

/**
 * 返回信息
 *
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


}
