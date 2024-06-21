package semios.api.model.vo.res.BaseWorkVo;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;

/**
 * 1.3 topup余额展示
 *
 * @description: topup balance details
 * @author: zhyyao
 * @create: 2023-11-21 11:20
 **/
@Data
@Slf4j
public class WorkNftDetailsVo {
    /**
     * 绑定的work id
     *
     * @ignore
     */
    private Integer workId;

    /**
     * 绑定的work编号
     *
     * @ignore
     */
    private Integer workNumber;

    /**
     * Work锁定状态 0-未锁定 1-已锁定
     *
     * @ignore
     */
    private Integer workLockStatus;


    /**
     * 绑定的work的dao id
     */
    private Integer daoId;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * topup的eth收入
     */
    private BigDecimal ethBalance = BigDecimal.ZERO;

    /**
     * topup的erc20收益
     */
    private BigDecimal tokenBalance = BigDecimal.ZERO;

    /**
     * Work聚合Dao id
     */
    private Integer togetherDaoId;

    /**
     * 1.7 所属的支付货币类型
     */
    private String payCurrencyType;


    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 work所属的input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * project对应的erc20 token地址
     */
    private String daoErc20Address;

}
