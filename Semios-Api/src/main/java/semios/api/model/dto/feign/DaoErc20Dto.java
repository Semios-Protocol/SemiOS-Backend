package semios.api.model.dto.feign;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;

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
     * projectId
     */
    private String projectId;

    /**
     * feePool地址
     */
    private String feePool;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;



    /**
     * 已发放量减Burn的数量
     */
    public BigDecimal getCirculatingSupply() {
        if (distributedSupply == null) {
            distributedSupply = BigDecimal.ZERO;
        }
        if (burnVolume == null) {
            burnVolume = BigDecimal.ZERO;
        }
        return distributedSupply.subtract(burnVolume);
    }

    /**
     * 总发放量减去Burn的数量
     */
    public BigDecimal getValuableSupply() {
        if (totalSupply == null) {
            totalSupply = BigDecimal.ZERO;
        }
        if (burnVolume == null) {
            burnVolume = BigDecimal.ZERO;
        }
        return totalSupply.subtract(burnVolume);
    }

    /**
     * 版税二次交易收益
     */
    private BigDecimal royaltyFeeIncome;

    public static DaoErc20Dto transfer(Dao dao) {
        DaoErc20Dto erc20SearchResVo = new DaoErc20Dto();
        erc20SearchResVo.setDaoId(dao.getId() + "");
        erc20SearchResVo.setErc20Address(dao.getErc20Token());
        erc20SearchResVo.setDaoName(dao.getDaoName());
        erc20SearchResVo.setDaoLogoUrl(dao.getDaoLogoUrl());
        erc20SearchResVo.setProjectId(dao.getProjectId());
        erc20SearchResVo.setFeePool(dao.getFeePool());
        erc20SearchResVo.setDaoAssetPool(dao.getFeePool());
        erc20SearchResVo.setEthInPool(dao.getDaoAssetPool());

        erc20SearchResVo.setPayCurrencyType(dao.getPayCurrencyType());
        erc20SearchResVo.setInputTokenAddress(dao.getInputTokenAddress());
        erc20SearchResVo.setInputTokenDecimals(dao.getInputTokenDecimals());


        if (StringUtils.isNotBlank(dao.getErc20TotalSupply())) {
            erc20SearchResVo.setTotalSupply(new BigDecimal(dao.getErc20TotalSupply()));
        }
        if (dao.getDaoReward() != null) {
            erc20SearchResVo.setDistributedSupply(dao.getDaoReward());
        }
        if (dao.getBurnAmount() != null) {
            erc20SearchResVo.setBurnVolume(dao.getBurnAmount());
        }
        if (dao.getRoyaltyFeeIncome() != null) {
            erc20SearchResVo.setRoyaltyFeeIncome(dao.getRoyaltyFeeIncome());
        }
        return erc20SearchResVo;
    }

}
