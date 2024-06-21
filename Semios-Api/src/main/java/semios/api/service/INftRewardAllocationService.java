package semios.api.service;


import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.NftRewardAllocation;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardDetailVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardVo;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * nft奖励记录表 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
public interface INftRewardAllocationService extends IService<NftRewardAllocation> {


    NftRewardAllocation getNftRewardAllocationByInfo(Integer daoId, Integer workId, String planCode);

    BigDecimal getTotalRewardByPlanCode(String planCode);

    List<UserTopupRewardVo> selectUserTopupRewardVo(String address);

    List<UserTopupRewardDetailVo> selectUserTopupRewardDetailVo(Integer daoId, String address);

    List<UserTopupRewardDetailVo> selectUserTopupRewardDetailVo(String projectId, String address);
}
