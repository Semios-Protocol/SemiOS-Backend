package semios.api.model.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * nft plan奖励分配记录
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("collect_record")
public class CollectRecord implements Serializable {

    private static final long serialVersionUID = 1L;

    //  id
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    //  聚合dao id
    private Integer daoId;

    //  聚合dao project id
    private String projectId;

    //  合约生成的planId
    private String planCode;

    //  nft的work id
    private Integer workId;

    //  erc721 address
    private String erc721Address;

    //  work number
    private Integer workNumber;

    //  记录的交易hash
    private String transactionHash;

    //  领取的address
    private String receivedAddress;

    //  领取的collect的数量
    private BigDecimal collectAmount;

    //  collect交易发起方
    private String createBy;

    //  创建时间
    @TableField(fill = FieldFill.INSERT)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;


}
