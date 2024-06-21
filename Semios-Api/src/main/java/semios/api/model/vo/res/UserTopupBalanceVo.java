package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;

/**
 * 1.3 topup余额展示
 *
 * @description: topup balance
 * @author: xiangbin
 * @create: 2023-11-21 11:20
 **/
@Data
@Slf4j
public class UserTopupBalanceVo extends DaoListVo {

    /**
     * topup的eth收入
     */
    private BigDecimal ethBalance = BigDecimal.ZERO;

    /**
     * topup的erc20收益
     */
    private BigDecimal tokenBalance = BigDecimal.ZERO;


    /**
     * topup Balance页面，显示on chain的 eth balance
     *
     * @mock 100000
     */
    private BigDecimal onChainEthBalance = BigDecimal.ZERO;


    /**
     * topup Balance页面，显示on chain的 token balance
     *
     * @mock 100000
     */
    private BigDecimal onChainTokenBalance = BigDecimal.ZERO;

    /**
     * 是否可以更新balance
     *
     * @mock false
     */
    private Boolean isUpdateBalance = false;


}
