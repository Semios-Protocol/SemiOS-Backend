package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.NftRewardAllocationMapper;
import semios.api.model.entity.NftRewardAllocation;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardDetailVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardVo;
import semios.api.service.INftRewardAllocationService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * nft奖励记录表 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-04-29
 */
@Service
public class NftRewardAllocationServiceImpl extends ServiceImpl<NftRewardAllocationMapper, NftRewardAllocation> implements INftRewardAllocationService {

    @Autowired
    private NftRewardAllocationMapper nftRewardAllocationMapper;


    @Override
    public NftRewardAllocation getNftRewardAllocationByInfo(Integer daoId, Integer workId, String planCode) {
        return nftRewardAllocationMapper.getNftRewardAllocationByInfo(daoId, workId, planCode);
    }

    @Override
    public BigDecimal getTotalRewardByPlanCode(String planCode) {
        return nftRewardAllocationMapper.getTotalRewardByPlanCode(planCode);
    }


    @Override
    public List<UserTopupRewardVo> selectUserTopupRewardVo(String address) {
        return nftRewardAllocationMapper.selectUserTopupRewardVo(address);
    }

    @Override
    public List<UserTopupRewardDetailVo> selectUserTopupRewardDetailVo(Integer daoId, String address) {
        //return nftRewardAllocationMapper.selectUserTopupRewardDetailVo(daoId,address);
        return new ArrayList<>();
    }

    @Override
    public List<UserTopupRewardDetailVo> selectUserTopupRewardDetailVo(String projectId, String address) {
        return nftRewardAllocationMapper.selectUserTopupRewardDetailVo(projectId, address);
    }
}
