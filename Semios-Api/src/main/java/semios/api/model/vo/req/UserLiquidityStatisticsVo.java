package semios.api.model.vo.req;

import lombok.Data;

import java.io.Serializable;

/**
 * <p>
 * 用户代币拥有量 更新最新代币数量
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Data
public class UserLiquidityStatisticsVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * 用户地址
     */
    private String userAddress;

    /**
     * 是否需要同步erc20Balance 0-否 1-是
     */
    private Integer syncErc20Balance;
}
