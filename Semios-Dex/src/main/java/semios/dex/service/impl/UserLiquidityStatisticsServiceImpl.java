package semios.dex.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.dex.mapper.UserLiquidityStatisticsMapper;
import semios.dex.model.entity.UserLiquidityStatistics;
import semios.dex.service.IUserLiquidityStatisticsService;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 用户代币拥有量 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class UserLiquidityStatisticsServiceImpl extends
        ServiceImpl<UserLiquidityStatisticsMapper, UserLiquidityStatistics> implements IUserLiquidityStatisticsService {

    @Autowired
    private UserLiquidityStatisticsMapper userLiquidityStatisticsMapper;

    @Override
    public UserLiquidityStatistics selectByUserAddress(String userAddress, String erc20Address) {
        return userLiquidityStatisticsMapper.selectByUserAddress(userAddress, erc20Address);
    }

    @Override
    public int updateSyncErc20Balance(String userAddress, String erc20Address, Integer syncErc20Balance) {
        return userLiquidityStatisticsMapper.updateSyncErc20Balance(userAddress, erc20Address, syncErc20Balance);
    }

    @Override
    public List<UserLiquidityStatistics> needSyncErc20Balance() {
        return userLiquidityStatisticsMapper.needSyncErc20Balance();
    }

    @Override
    public List<UserLiquidityStatistics> getLiquidityStatisticsByErc20Address(String erc20Address) {
        return userLiquidityStatisticsMapper.getLiquidityStatisticsByErc20Address(erc20Address);
    }

    @Override
    public BigDecimal getSumBalanceByErc20Address(String erc20Address) {
        return userLiquidityStatisticsMapper.getSumBalanceByErc20Address(erc20Address);
    }

    @Override
    public List<UserLiquidityStatistics> needSyncIsContract() {
        return userLiquidityStatisticsMapper.needSyncIsContract();
    }

    @Override
    public UserLiquidityStatistics selectHaveErc20UserAddress(String erc20Address) {
        return userLiquidityStatisticsMapper.selectHaveErc20UserAddress(erc20Address);
    }

    @Override
    public Integer erc20OwnerAmount(String erc20Address) {
        return userLiquidityStatisticsMapper.erc20OwnerAmount(erc20Address);
    }
}
