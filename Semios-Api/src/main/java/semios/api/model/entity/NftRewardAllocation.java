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
 * nft奖励记录表
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("nft_reward_allocation")
public class NftRewardAllocation implements Serializable {

    private static final long serialVersionUID = 1L;

    //  id
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    //  聚合dao id
    private Integer daoId;

    //  聚合dao project id
    private String projectId;

    //  nft的work id
    private Integer workId;

    //  erc721 address
    private String erc721Address;

    //  work number
    private Integer workNumber;

    //  合约生成的planId
    private String planCode;

    //  可以collect的数量
    private BigDecimal planRewardAmount;

    //  创建时间
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;

}
