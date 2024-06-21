package semios.dex.model.entity;

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
 * 订阅记录
 * </p>
 *
 * @author xiangbin
 */
@Data
public class Subscribe implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 合约地址
     */
    private String contractAddress;

    /**
     * 订阅的主题或者方法名
     */
    private String topics;

    /**
     * 开始块高度
     */
    private String fromBlock;

    /**
     * 接收回调地址
     */
    private String receiveAddress;

    /**
     * 订阅ID
     */
    private String filterId;

    /**
     * 创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;

    private String tradeType;

    private Integer isDel;

    private Integer status;

    private Integer orderInit;


}
