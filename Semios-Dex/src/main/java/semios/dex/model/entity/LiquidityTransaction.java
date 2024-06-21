package semios.dex.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * <p>
 * 流通性交易表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class LiquidityTransaction implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * erc20名称
     */
    private String erc20Name;

    /**
     * erc20Symbol
     */
    private String erc20Symbol;

    /**
     * 交易类型1-swapErc20 2-swapEth 3-add 4-remove 5-burn
     */
    private Integer tradeType;

    /**
     * 交易链上hash
     */
    private String transactionHash;

    /**
     * 上链时间
     */
    private Long blockTime;

    /**
     * 交易时使用的代币 add、remove、burn固定是erc20 swapForEth为erc20 swapForToken为eth数量
     */
    private BigDecimal inTokenAmount;

    /**
     * 交易时兑换出来的代币 add、remove、burn固定是eth swapForEth为eth swapForToken为erc20数量
     */
    private BigDecimal outTokenAmount;

    /**
     * erc20总余额
     */
    private BigDecimal erc20Balance;

    /**
     * eth总余额
     */
    private BigDecimal ethBalance;

    /**
     * 本次eth交易量
     */
    private BigDecimal ethAmount;

    /**
     * 添加和减少流动性时的pool token数量
     */
    private BigDecimal poolTokenAmount;

    /**
     * 交易的用户地址
     */
    private String userAddress;

    /**
     * 创建时间
     */
    private Timestamp createTime;

    /**
     * 更新时间
     */
    private Timestamp modifyTime;
}
