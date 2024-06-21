package semios.api.model.entity;

import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;

import java.time.LocalDateTime;
import java.io.Serializable;

/**
 * <p>
 * topup模式work下的eth和token的数量
 * </p>
 *
 * @author zhyyao
 * @since 2024-01-30
 */
@TableName("work_topup_harvest")
@Data
public class WorkTopupHarvest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 铸造nft的work id
     */
    private Integer workId;

    /**
     * on chain input token
     */
    private BigDecimal inputTokenAmount;

    /**
     * on chain out put token
     */
    private BigDecimal outputTokenAmount;

    /**
     * 铸造nft所属的 主dao id
     */
    private Integer daoId;

    /**
     * 铸造nft所属的主dao的projectId
     */
    private String projectId;

    /**
     * 铸造nft的erc20余额
     */
    private BigDecimal erc20Amount;

    /**
     * 铸造nft的eth余额
     */
    private BigDecimal ethAmount;

    /**
     * 消费dao的project对应的erc721address地址
     */
    private String mountErc721Address;

    /**
     * 消费的work id
     */
    private Integer mountWorkId;

    /**
     * 消费的work编号
     */
    private Integer mountWorkNumber;

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
