package semios.api.model.entity;

import java.io.Serializable;
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
 * 停机记录表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("shutdown_record")
public class ShutdownRecord implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 0-平台停机 1-dao停机 2-canvas停机
     */
    private Integer type;

    /**
     * daoid或者canvasid
     */
    private Integer recordId;

    /**
     * 区块号
     */
    private String blockNumber;

    /**
     * 领取时间
     */
    private String blockTime;

    /**
     * 交易hash 创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;

    /**
     * 是否停机 0-停机 1-启动
     */
    private Integer isPaused;

    /**
     * 铸造时所属drb区块
     */
    private Integer drbNumber;

    /**
     * Work铸造时的交易hash，默认为空
     */
    private String transactionHash;

    /**
     * 0-无效 1-有效
     */
    private Integer isValid;

    /**
     * projetId或者canvasId
     */
    private String shutdownId;

    /**
     * 是否删除 0-未删除 1-已删除
     */
    private Integer isDel;

}
