package semios.dex.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * erc20流通性表
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class Erc20Liquidity implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(type = IdType.AUTO)
    private Integer id;

    /**
     * dao_id
     */
    private Integer daoId;

    /**
     * projectId
     */
    private String projectId;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * erc721地址
     */
    private String erc721Address;

    /**
     * erc20名称
     */
    private String erc20Name;

    /**
     * erc20 symbol
     */
    private String erc20Symbol;

    /**
     * erc20上链时间
     */
    private Long erc20BlockTime;

    /**
     * dao状态 0-未开始 1-已开始
     */
    private Integer daoStatus;

    /**
     * eth地址
     */
    private String ethAddress;

    /**
     * 交易对地址
     */
    private String pairAddress;

    /**
     * 创建交易对时erc20的顺序为第0个还是第1个
     */
    private Integer erc20Order;

    /**
     * pair token总余额
     */
    private BigDecimal pairBalance;

    /**
     * erc20总余额
     */
    private BigDecimal erc20Balance;

    /**
     * eth总余额
     */
    private BigDecimal ethBalance;

    /**
     * 交易对开通链上hash
     */
    private String transactionHash;

    /**
     * 交易对开通上链时间
     */
    private Long blockTime;

    /**
     * 最近一次比例变更的hash
     */
    private String lastSwapHash;

    /**
     * 创建者用户地址
     */
    private String createAddress;

    /**
     * 是否删除 0-未删除 1-已删除
     */
    private Integer isDel;

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
    private LocalDateTime modifyTime;

    /**
     * ERC20的版税收益修改的交易hash
     */
    private String royaltyFeeModifyHash;

    /**
     * ERC20的版税收益
     */
    private BigDecimal royaltyFee;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * app来源 1-dao4art 2-protodao
     */
    private Integer appSource;
}
