package semios.api.model.entity;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import lombok.Data;

/**
 * <p>
 * 用户mint获得的代币
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("user_harvest_token")
public class UserHarvestToken implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * canvas 表id 仅随机存一个canvasId，目前一个dao只有一条记录
     */
    private Integer canvasId;

    /**
     * 获得token的总数量
     */
    @Deprecated
    private BigDecimal totalToken;

    /**
     * 已领取未使用的token数量
     */
    private BigDecimal receivedToken;

    /**
     * 未领取token数量
     */
    private BigDecimal unclaimedToken;

    /**
     * transfer token数量
     */
    private BigDecimal transferToken;

    /**
     * 已兑换token数量
     */
    private BigDecimal swapToken;

    /**
     * 已兑换eth数量
     */
    private BigDecimal swapEth;

    /**
     * 用户地址
     */
    private String userAddress;

    /**
     * 最后一次修改的hash
     */
    private String lastTransactionHash;

    /**
     * 已领取未使用的eth数量
     */
    private BigDecimal receivedEth;

    /**
     * 未领取eth数量
     */
    private BigDecimal unclaimedEth;

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
    private LocalDateTime modifyTime;

}
