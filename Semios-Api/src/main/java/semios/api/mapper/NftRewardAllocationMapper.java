package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.NftRewardAllocation;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardDetailVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardVo;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * nft奖励记录表 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
public interface NftRewardAllocationMapper extends BaseMapper<NftRewardAllocation> {

    @Select("select * from nft_reward_allocation where dao_id=#{daoId} and work_id=#{workId} and plan_code=#{planCode};")
    NftRewardAllocation getNftRewardAllocationByInfo(Integer daoId, Integer workId, String planCode);


    @Select("select SUM(plan_reward_amount) from nft_reward_allocation where plan_code=#{planCode};")
    BigDecimal getTotalRewardByPlanCode(String planCode);

    @Select("select nra.dao_id as daoId, any_value(nra.project_id) as projectId,any_value(d.dao_name) as daoName, " +
            " any_value(d.dao_logo_url) as daoLogoUrl , any_value(d.dao_symbol) as daoSymbol, any_value(d.pay_currency_type) as payCurrencyType, any_value(d.erc20_token) as daoErc20Address, ifnull(sum(nra.plan_reward_amount),0) as totalCollectable " +
            " from nft_reward_allocation nra " +
            "  left join work w on nra.work_id = w.id " +
            "  left join dao d on nra.dao_id = d.id " +
            "  where w.owner_address=#{address} group by nra.dao_id order by ifnull(sum(nra.plan_reward_amount),0) desc;")
    List<UserTopupRewardVo> selectUserTopupRewardVo(String address);


//    @Select("select ip.id as planId,any_value(ip.plan_number) as planNumber,sum(nra.plan_reward_amount) as collectedAmount from nft_reward_allocation nra " +
//            "  left join work w on nra.work_id = w.id " +
//            "  left join incentive_plan ip on nra.plan_code = ip.plan_code " +
//            "  where nra.dao_id=#{daoId} and w.owner_address=#{address} and nra.plan_reward_amount>0 group by ip.id order by ip.id desc ")
//    List<UserTopupRewardDetailVo> selectUserTopupRewardDetailVo(Integer daoId,String address);


    @Select("select ip.id as planId,any_value(ip.plan_number) as planNumber,ip.plan_code as planCode,any_value(ip.reward_token_symbol) as rewardTokenSymbol,any_value(ip.reward_token) as rewardToken,sum(nra.plan_reward_amount) as collectableAmount from nft_reward_allocation nra " +
            "  left join work w on nra.work_id = w.id " +
            "  left join incentive_plan ip on nra.plan_code = ip.plan_code " +
            " where nra.project_id=#{projectId} and w.owner_address=#{address}  group by ip.id order by ip.id desc ")
    List<UserTopupRewardDetailVo> selectUserTopupRewardDetailVo(String projectId, String address);
}
