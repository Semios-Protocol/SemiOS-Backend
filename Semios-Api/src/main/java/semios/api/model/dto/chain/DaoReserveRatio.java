package semios.api.model.dto.chain;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @description: 铸造收益分配策略
 * @author: xiangbin
 * @create: 2023-06-12 13:55
 **/
@Data
public class DaoReserveRatio {

    /**
     * d4a分配比例 PAO Mint Fee
     */
    private BigDecimal d4aMintFee;
    /**
     * dao分配比例 SubDAO Mint Fee
     */
    private BigDecimal daoMintFee;

    /**
     * dao分配比例 MainDAO Mint Fee
     */
    private BigDecimal redeemPoolMintFee;

    /**
     * canvas分配比例 Builder Mint Fee
     */
    private BigDecimal canvasMintFee;

    /**
     * 默认非一口价
     */
    public DaoReserveRatio() {
        this.d4aMintFee = new BigDecimal("2.5");
        this.daoMintFee = new BigDecimal("30");
        this.canvasMintFee = new BigDecimal("67.5");
    }

    /**
     * @param fix 是否一口价 true-是 false-否
     */
    public DaoReserveRatio(boolean fix) {
        if (fix) {
            this.d4aMintFee = new BigDecimal("2.5");
            this.daoMintFee = new BigDecimal("35");
            this.canvasMintFee = new BigDecimal("62.5");
        } else {
            this.d4aMintFee = new BigDecimal("2.5");
            this.daoMintFee = new BigDecimal("30");
            this.canvasMintFee = new BigDecimal("67.5");
        }
    }

//    public void setDaoMintFee(BigDecimal daoMintFee) {
//        this.daoMintFee = daoMintFee;
//        this.canvasMintFee =
//                BigDecimal.ONE.multiply(new BigDecimal("100")).subtract(this.d4aMintFee).subtract(daoMintFee);
//    }
}
