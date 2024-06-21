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
 * 订阅记录表
 * </p>
 *
 * @author xiangbin
 */
@Data
public class Subscriber implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 网络Mainnet Ropsten
     */
    private String network;

    /**
     * 开始块高度 为空则从当前块开始
     */
    private String fromBlock;

    /**
     * 监听地址
     */
    private String address;

    /**
     * 监听主题
     */
    private String topics;

    /**
     * 间隔时间 秒s 目前只有10和60两个类型和对应的定时任务
     */
    private Integer intervalTime;

    /**
     * 通知地址
     */
    private String noticeUrl;

    /**
     * 通知异常次数超过10次则停止通知
     */
    private Integer noticeErrTimes;

    /**
     * 监听状态 0-关闭 1-开启
     */
    private Integer subStatus;

    /**
     * 删除标识 0-未删除 1-已删除
     */
    private Integer isDel;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createTime;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime updateTime;

    private Integer noticeType;

    /**
     * 应用名称
     */
    private String appName;


}
