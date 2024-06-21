package semios.dex.model.vo.req;

import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * erc20流通性表
 * </p>
 *
 * @author xiangbin
 */
@Data
public class Erc20LiquidityReqVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * dao_id
     */
    private Integer daoId;

    /**
     * dao状态 0-未开始 1-已开始
     */
    private Integer daoStatus;

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
    private String erc20BlockTime;

    /**
     * eth地址
     */
    private String ethAddress;

    /**
     * 交易对地址
     */
    private String pairAddress;

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
    private String blockTime;

    /**
     * 最近一次比例变更的hash
     */
    private String lastSwapHash;

    /**
     * 创建者用户地址
     */
    private String createAddress;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;
}
