package semios.dex.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * erc20流通性每日零点统计数据
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class LiquidityDailyStatistics implements Serializable {

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
     * 当前drb的值
     */
    private Integer drbNumber;

    /**
     * 交易erc总20数量
     */
    private BigDecimal erc20Amount;

    /**
     * 交易eth总数量
     */
    private BigDecimal ethAmount;

    /**
     * 兑换eth比例
     */
    private BigDecimal ethSwappedRate;

    /**
     * 兑换erc20比例
     */
    private BigDecimal erc20SwappedRate;

    /**
     * APY
     */
    private BigDecimal apy;

    /**
     * burnPrice
     */
    private BigDecimal burnPrice;

    /**
     * 资金池内总金额
     */
    private BigDecimal assetPoolTotalAmount;

    /**
     * 资金池收入
     */
    private BigDecimal assetPoolIncome;

    /**
     * 资金池支出
     */
    private BigDecimal assetPoolCost;

    /**
     * 资金池变化量
     */
    private BigDecimal assetPoolVariation;

    /**
     * 记录时间戳
     */
    private Long recordTime;

    /**
     * 更新时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime modifyTime;
}
