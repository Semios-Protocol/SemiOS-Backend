package semios.api.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;
import semios.api.model.enums.StatisticsStatusEnum;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * Dao资金池每日零点统计数据
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("dao_daily_statistics")
public class DaoDailyStatistics implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * dao的id
     */
    private Integer daoId;

    /**
     * dao的projectid
     */
    private String projectId;

    /**
     * 资金池内总金额
     */
    private BigDecimal assetPoolTokenTotalAmount;

    /**
     * 资金池收入
     */
    private BigDecimal assetPoolTokenIncome;

    /**
     * 资金池支出
     */
    private BigDecimal assetPoolTokenCost;

    /**
     * 资金池变化量
     */
    private BigDecimal assetPoolTokenVariation;

    /**
     * 资金池内总金额
     */
    private BigDecimal assetPoolEthTotalAmount;

    /**
     * 资金池收入
     */
    private BigDecimal assetPoolEthIncome;

    /**
     * 资金池支出
     */
    private BigDecimal assetPoolEthCost;

    /**
     * 资金池变化量
     */
    private BigDecimal assetPoolEthVariation;


    /**
     * 计算状态 0-未计算 1-计算中 2-计算完成
     *
     * @see StatisticsStatusEnum
     */
    private Integer status;


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
