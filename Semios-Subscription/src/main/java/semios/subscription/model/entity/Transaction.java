package semios.subscription.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * <p>
 * transaction记录表
 * </p>
 *
 * @author xiangbin
 * @create: 2022-04-14 13:41
 */
@Data
public class Transaction implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    private String address;

    private String blockHash;

    private String blockNumber;

    private Integer blockIntNum;

    private String data;

    private String logIndex;

    private String removed;

    private String topics;

    private String transactionHash;

    private String transactionIndex;

    private Integer subId;

    /**
     * 通知次数
     */
    private Integer noticeTimes;

    /**
     * 通知状态 0-未成功 1-已成功
     */
    private Integer noticeStatus;

    /**
     * 创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;


    private String blockTimestamp;

    /**
     * 应用名称
     */
    private String appName;


}
