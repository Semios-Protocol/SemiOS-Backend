package semios.dex.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * <p>
 * erc20流通性价格变更记录
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class LiquidityPriceRecord implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * 类型 0-swap交易量 1-burn交易量
     */
    private Integer type;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * 交易erc总20数量
     */
    private BigDecimal erc20Amount;

    /**
     * 交易eth总数量
     */
    private BigDecimal ethAmount;

    /**
     * 交易量
     */
    private BigDecimal tradingVolume;

    /**
     * 交易价格 都是token兑换eth价格
     */
    private BigDecimal price;

    /**
     * 记录时间戳
     */
    private Long recordTime;

    /**
     * 更新时间
     */
    private Timestamp modifyTime;
}
