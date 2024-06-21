package semios.dex.model.dto.feign;

import lombok.Data;

import java.math.BigDecimal;

/**
 * Dao erc20 info
 *
 * @author: fjtan
 * @create: 2023-05-21 12:14
 **/
@Data
public class DaoErc20Dto {

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * daoId
     */
    private String daoId;
    /**
     * DAO名称
     */
    private String daoName;

    /**
     * DAOlogo地址
     */
    private String daoLogoUrl;

    /**
     * project对应的asset pool地址
     */
    private String daoAssetPool;

    /**
     * asset pool中eth量
     */
    private BigDecimal ethInPool = BigDecimal.ZERO;

    /**
     * ERC20总发放量
     */
    private BigDecimal totalSupply = BigDecimal.ZERO;

    /**
     * ERC20已发放量
     */
    private BigDecimal distributedSupply = BigDecimal.ZERO;

    /**
     * ERC20已经Burn的数量
     */
    private BigDecimal burnVolume = BigDecimal.ZERO;

    /**
     * 版税二次交易收益
     */
    private BigDecimal royaltyFeeIncome = BigDecimal.ZERO;

    /**
     * projectId
     */
    private String projectId;

    /**
     * feePool地址
     */
    private String feePool;

    /**
     * 已发放量减Burn的数量
     */
    public BigDecimal getCirculatingSupply() {
        return distributedSupply.subtract(burnVolume);
    }

    /**
     * 总发放量减去Burn的数量
     */
    public BigDecimal getValuableSupply() {
        return totalSupply.subtract(burnVolume);
    }

}
