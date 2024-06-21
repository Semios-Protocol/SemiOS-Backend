package semios.api.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
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
 * topup模式用户拥有eth和token的数量
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("user_topup_harvest")
public class UserTopupHarvest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 用户地址
     */
    private String userAddress;

    /**
     * mainDao dao id
     */
    private Integer daoId;

    /**
     * mainDao projectId
     */
    private String projectId;

    /**
     * erc20Token地址
     */
    private String subdaoErc20Token;

    /**
     * erc20余额
     */
    private BigDecimal erc20Amount;

    /**
     * eth余额
     */
    private BigDecimal ethAmount;

    /**
     * 分配时的drb
     */
    private Integer roundDrb;

    /**
     * 0-未删除 1-已删除
     */
    private Integer isDel;

    /**
     * 创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime updateTime;

}
