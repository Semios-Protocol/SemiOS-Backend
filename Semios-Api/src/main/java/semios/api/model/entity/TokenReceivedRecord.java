package semios.api.model.entity;

import java.io.Serializable;
import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;

import lombok.Data;

/**
 * <p>
 * 代币领取记录表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
@TableName("token_received_record")
public class TokenReceivedRecord implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 0-dao token 1-eth
     */
    private Integer type;

    /**
     * 领取token数量
     */
    private BigDecimal tokenNum;

    /**
     * 领取eth数量
     */
    private BigDecimal ethNum;

    /**
     * 领取类型1-dao领取 2-canvas领取 3-transfer 4-mint领取 5-解锁erc20
     */
    private Integer receiveType;

    /**
     * daoid或者canvasid
     */
    private Integer receiveId;

    /**
     * 领取人地址
     */
    private String receiveAddress;

    /**
     * dao的projectid
     */
    private String projectId;

    /**
     * canvas的id
     */
    private String canvasId;

    /**
     * 区块号
     */
    private String blockNumber;

    /**
     * 领取时间
     */
    private String blockTime;

    /**
     * 交易hash
     */
    private String transactionHash;

    /**
     * 属于某个drb
     */
    private Integer drbNumber;

    /**
     * 交易类型0-collect 1-swap 2-transfer 3-unlock
     */
    private Integer tokenType;

    /**
     * from的地址
     */
    private String fromAddress;

    /**
     * to的地址,发起交易的address
     */
    private String toAddress;

    /**
     * 兑换获得的eth数量
     */
    private BigDecimal ethAmount;

    /**
     * dao num
     */
    private Integer daoNumber;

    /**
     * transfer的token的剩余数量
     */
    private BigDecimal tokenNumBalance;

    /**
     * 是否同步dex,只有swap需要同步 0-未同步 1-已同步'
     */
    private Integer syncDex = 1;
}
