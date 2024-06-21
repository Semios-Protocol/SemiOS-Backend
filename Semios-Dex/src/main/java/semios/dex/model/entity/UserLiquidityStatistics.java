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
 * 用户代币拥有量
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class UserLiquidityStatistics implements Serializable {

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
     * 用户地址
     */
    private String userAddress;

    /**
     * 用户id
     */
    private String userId;

    /**
     * 用户logo
     */
    private String userLogo;

    /**
     * 是否为合约地址 0-否 1-是
     */
    private Integer isContract;

    /**
     * erc20总量
     */
    private BigDecimal erc20Balance;

    /**
     * pool token数量
     */
    private BigDecimal poolToken;

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

    /**
     * 是否需要同步erc20Balance 0-否 1-是
     */
    private Integer syncErc20Balance;
}
