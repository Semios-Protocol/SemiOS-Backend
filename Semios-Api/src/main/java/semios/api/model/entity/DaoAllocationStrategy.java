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
 * dao分配策略表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("dao_allocation_strategy")
public class DaoAllocationStrategy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 分配的dao的原始projectid
     */
    private String originProjectId;

    /**
     * 0-dao token 1-eth
     */
    private Integer type;

    /**
     * 分配类型 0-非当前dao 1-redeem Asset Pool 2-selfReward 3-不出块比例
     * DaoRoyaltyTypeEnum
     */
    private Integer royaltyType;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao的projectid
     */
    private String projectId;

    /**
     * DAO名称
     */
    private String daoName;

    /**
     * dao编号
     */
    private Integer daoNumber;

    /**
     * 分配比例
     */
    private BigDecimal royaltyProportion;

    /**
     * 设置比例时交易hash
     */
    private String transactionHash;

    /**
     * 设置比例时交易hash上链时间
     */
    private String blockTime;

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
