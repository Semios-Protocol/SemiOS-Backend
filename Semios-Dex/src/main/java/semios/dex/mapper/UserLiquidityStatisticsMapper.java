package semios.dex.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;
import semios.dex.model.entity.UserLiquidityStatistics;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 用户代币拥有量 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface UserLiquidityStatisticsMapper extends BaseMapper<UserLiquidityStatistics> {

    @Select("select * from user_liquidity_statistics where user_address = #{userAddress} and erc20_address = #{erc20Address} limit 1")
    UserLiquidityStatistics selectByUserAddress(String userAddress, String erc20Address);

    @Update("update user_liquidity_statistics  set sync_erc20_balance = #{syncErc20Balance}  where user_address = #{userAddress} and erc20_address = #{erc20Address}")
    int updateSyncErc20Balance(String userAddress, String erc20Address, Integer syncErc20Balance);

    @Select("select * from user_liquidity_statistics where sync_erc20_balance = 1")
    List<UserLiquidityStatistics> needSyncErc20Balance();

    @Select("select * from user_liquidity_statistics where erc20_address = #{erc20Address} and erc20_balance > 0 order by erc20_balance desc limit 100")
    List<UserLiquidityStatistics> getLiquidityStatisticsByErc20Address(String erc20Address);

    @Select("select sum(erc20_balance) from user_liquidity_statistics where erc20_address = #{erc20Address}")
    BigDecimal getSumBalanceByErc20Address(String erc20Address);


    @Select("select * from user_liquidity_statistics where is_contract is null")
    List<UserLiquidityStatistics> needSyncIsContract();

    @Select("select * from user_liquidity_statistics where erc20_address = #{erc20Address} and erc20_balance > 0 and is_contract = 0 limit 1")
    UserLiquidityStatistics selectHaveErc20UserAddress(String erc20Address);

    @Select("select sum(1) from user_liquidity_statistics where erc20_address = #{erc20Address} and erc20_balance > 0")
    Integer erc20OwnerAmount(String erc20Address);

}
