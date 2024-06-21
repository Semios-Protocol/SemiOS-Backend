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
 * dao分配数量表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("dao_allocation_amount")
public class DaoAllocationAmount implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 0-daoToken分配 1-eth分配
     */
    private Integer type;

    /**
     * 0-其他dao分配 1-当前dao分配 2-当前dao的redeem池分配
     */
    private Integer allocationType;

    /**
     * 分配的fromDao的projectId
     */
    private String fromDaoId;

    /**
     * 分配的toDaoId的projectId
     */
    private String toDaoId;

    /**
     * token地址 0x0代表eth分配
     */
    private String token;

    /**
     * toDao的dao表的id
     */
    private Integer toDid;

    /**
     * toDao的DAO名称
     */
    private String toDaoName;

    /**
     * toDao的dao编号
     */
    private Integer toDaoNumber;

    /**
     * 分配的数量
     */
    private BigDecimal amount;

    /**
     * 分配时的drb
     */
    private Integer roundDrb;

    /**
     * 交易hash
     */
    private String transactionHash;

    /**
     * 交易hash上链时间
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
