package semios.api.model.vo.res;

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
public class UserTopupBalanceDetailsVo {
    /**
     * work id
     */
    private Integer workId;

    /**
     * work id
     */
    private Integer daoId;

    /**
     * work编号
     */
    private Integer workNumber;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * Work锁定状态 0-未锁定 1-已锁定
     */
    private Integer workLockStatus;

    /**
     * topup的eth收入
     */
    private BigDecimal ethBalance = BigDecimal.ZERO;

    /**
     * topup的erc20收益
     */
    private BigDecimal tokenBalance = BigDecimal.ZERO;

    /**
     * 聚合Dao id
     */
    private Integer togetherDaoId;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;


    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;


    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * project对应的erc20 token地址
     */
    private String daoErc20Address;

}
