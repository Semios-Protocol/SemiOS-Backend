package semios.api.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * dao代币追加记录
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("dao_append_token_log")
public class DaoAppendTokenLog implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * 原始dao代币数量
     */
    private BigDecimal originTotalSupply;

    /**
     * 最新代币数量
     */
    private BigDecimal newTotalSupply;

    /**
     * 追加人地址，非第三方时为空
     */
    private String userAddress;

    /**
     * 是否为外部ERC20 0-否 1-是
     */
    private Integer isThirdparty;

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
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    private LocalDateTime updateTime;


}
