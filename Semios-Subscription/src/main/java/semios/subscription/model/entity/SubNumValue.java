package semios.subscription.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * <p>
 * 数值类型订阅最新值
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("sub_num_value")
public class SubNumValue implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 合约地址
     */
    private String address;

    /**
     * netWork
     */
    private String netWork;

    /**
     * 订阅的方法
     */
    private String topic;

    /**
     * 区块高度
     */
    private String blockHeight;

    /**
     * 区块上的值
     */
    private String value;

    /**
     * 更新时间
     */
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime updateTime;

    /**
     * filter_id
     */
    private Integer filterId;

}
