package semios.dex.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.dex.model.entity.UserLiquidityStatistics;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 用户代币拥有量 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IUserLiquidityStatisticsService extends IService<UserLiquidityStatistics> {

    UserLiquidityStatistics selectByUserAddress(String userAddress, String erc20Address);

    int updateSyncErc20Balance(String userAddress, String erc20Address, Integer syncErc20Balance);

    List<UserLiquidityStatistics> needSyncErc20Balance();

    List<UserLiquidityStatistics> getLiquidityStatisticsByErc20Address(String erc20Address);

    BigDecimal getSumBalanceByErc20Address(String erc20Address);

    List<UserLiquidityStatistics> needSyncIsContract();

    UserLiquidityStatistics selectHaveErc20UserAddress(String erc20Address);

    Integer erc20OwnerAmount(String erc20Address);
}
