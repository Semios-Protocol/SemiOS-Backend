package semios.dex.model.vo.res;

import lombok.Data;
import semios.dex.model.dto.common.DexConstant;
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.enums.LiquidityTradeTypeEnum;

import java.math.RoundingMode;
import java.util.Objects;

/**
 * Transactions
 *
 * @description: Transactions
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class TransactionResVo {

    /**
     * 交易类型1-swapErc20 2-swapEth 3-add 4-remove 5-burn
     *
     * @mock 3
     */
    private Integer tradeType;

    /**
     * 交易交易类别 Swaps ETH for D4A.T123
     *
     * @mock Swaps ETH for D4A.T123
     */
    private String tradeTypeDesc;

    /**
     * Txn hash
     *
     * @mock 0xa4a921b415a33c74afd723de285dcb669c8854eccd9990409777b8092c6c9c33
     */
    private String transactionHash;

    /**
     * In Token 交易时使用的代币 add、remove、burn固定是erc20
     *
     * @mock 0.11
     */
    private String inTokenAmount;

    /**
     * In Token Unit
     *
     * @mock ETH
     */
    private String inTokenUnit;

    /**
     * Out Token 交易时兑换出来的代币 add、remove、burn固定是eth
     *
     * @mock 10000
     */
    private String outTokenAmount;

    /**
     * Out Token Unit
     *
     * @mock D4A.T123
     */
    private String outTokenUnit;

    /**
     * 交易的价格 只有swap和burn的时候展示
     *
     * @mock 0.000011
     */
    private String swappedRate;

    /**
     * 交易的价格 只有swap和burn的时候展示
     *
     * @mock ETH
     */
    private String swappedRateUnit = DexConstant.ETH_UNIT;

    /**
     * 用户地址
     *
     * @mock 0xee370508e532f9a3c1c39fc1059b1c0ded6d0aa4
     */
    private String account;

    /**
     * 交易时间Time
     *
     * @mock 1674218183
     */
    private Long tradeTime;

    public static TransactionResVo transfer(LiquidityTransaction liquidityTransaction) {
        TransactionResVo transactionResVo = new TransactionResVo();
        transactionResVo.setTradeType(liquidityTransaction.getTradeType());
        transactionResVo.setTransactionHash(liquidityTransaction.getTransactionHash());
        transactionResVo.setInTokenAmount(liquidityTransaction.getInTokenAmount().stripTrailingZeros().toPlainString());
        transactionResVo
                .setOutTokenAmount(liquidityTransaction.getOutTokenAmount().stripTrailingZeros().toPlainString());
        if (Objects.equals(liquidityTransaction.getTradeType(), LiquidityTradeTypeEnum.SWAPERC20.getStatus())) {
            transactionResVo.setInTokenUnit(DexConstant.ETH_UNIT);
            transactionResVo.setOutTokenUnit(liquidityTransaction.getErc20Symbol());
            transactionResVo.setSwappedRate(liquidityTransaction.getInTokenAmount()
                    .divide(liquidityTransaction.getOutTokenAmount(), DexConstant.PRICE_DECIMAL, RoundingMode.HALF_UP)
                    .stripTrailingZeros().toPlainString());
        } else {
            transactionResVo.setInTokenUnit(liquidityTransaction.getErc20Symbol());
            transactionResVo.setOutTokenUnit(DexConstant.ETH_UNIT);
            transactionResVo.setSwappedRate(liquidityTransaction.getOutTokenAmount()
                    .divide(liquidityTransaction.getInTokenAmount(), DexConstant.PRICE_DECIMAL, RoundingMode.HALF_UP)
                    .stripTrailingZeros().toPlainString());
        }
        if (Objects.equals(liquidityTransaction.getTradeType(), LiquidityTradeTypeEnum.ADD.getStatus())
                || Objects.equals(liquidityTransaction.getTradeType(), LiquidityTradeTypeEnum.REMOVE.getStatus())) {
            transactionResVo.setTradeTypeDesc(LiquidityTradeTypeEnum.findEnumByCode(liquidityTransaction.getTradeType())
                    + " " + transactionResVo.getInTokenUnit() + " and " + transactionResVo.getOutTokenUnit());
        } else {
            transactionResVo.setTradeTypeDesc(LiquidityTradeTypeEnum.findEnumByCode(liquidityTransaction.getTradeType())
                    + " " + transactionResVo.getInTokenUnit() + " for " + transactionResVo.getOutTokenUnit());
        }
        transactionResVo.setAccount(liquidityTransaction.getUserAddress());
        transactionResVo.setTradeTime(liquidityTransaction.getBlockTime());
        return transactionResVo;
    }

}
