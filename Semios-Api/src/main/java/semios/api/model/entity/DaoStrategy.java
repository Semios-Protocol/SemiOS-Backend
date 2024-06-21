package semios.api.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;
import semios.api.model.enums.DaoStrategyStrategyTypeEnum;
import semios.api.model.enums.DaoStrategyTypeEnum;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * <p>
 * dao黑白名单策略表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("dao_strategy")
public class DaoStrategy implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 0-create_canvas 1-mint_work
     *
     * @see DaoStrategyTypeEnum
     */
    private Integer type;

    /**
     * 1-白名单 2-erc721白名单 3-黑名单
     *
     * @see DaoStrategyStrategyTypeEnum
     */
    private Integer strategyType;

    /**
     * erc721地址或者黑白名单地址 黑白名单地址以逗号分隔
     */
    private String originAddress;

    /**
     * 白名单地址时代表proof_store表id，erc721时代表daoId
     */
    private Integer proofId;

    /**
     * 交易hash
     */
    private String transactionHash;

    /**
     * 交易hash上链时间
     */
    private String blockTime;

    /**
     * dao的uri地址
     */
    private String daoUri;

    /**
     * dao的id
     */
    private Integer daoId;

    /**
     * dao的projectid
     */
    private String projectId;

    /**
     * dao的编号
     */
    private Integer daoNumber;

    /**
     * 0-未删除 1-已删除
     */
    private Integer isDel;

    /**
     * 0-无效 1-有效
     */
    private Integer isValid;

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
