package semios.api.service.impl;


import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import semios.api.mapper.WorkTopupHarvestMapper;
import semios.api.model.entity.WorkTopupHarvest;
import semios.api.model.vo.req.DaoProjectVo;
import semios.api.model.vo.res.*;
import semios.api.model.vo.res.TopUpReward.TopupNftListVo;
import semios.api.service.IWorkTopupHarvestService;

import javax.annotation.Resource;
import java.util.List;

/**
 * <p>
 * topup模式work下的eth和token的数量 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-01-30
 */
@Service
public class WorkTopupHarvestServiceImpl extends ServiceImpl<WorkTopupHarvestMapper, WorkTopupHarvest> implements IWorkTopupHarvestService {


    @Resource
    private WorkTopupHarvestMapper workTopupHarvestMapper;

    @Override
    public WorkTopupHarvest selectByProjectIdAndMountWorkId(String projectId, Integer mountWorkId) {
        return workTopupHarvestMapper.selectByProjectIdAndMountWorkId(projectId, mountWorkId);
    }

    @Override
    public List<WorkTopupHarvest> selectListByMountWorkIdAndMount721Address(Integer mountWorkId, String mount721Address) {
        return workTopupHarvestMapper.selectListByMountWorkIdAndMount721Address(mountWorkId, mount721Address);

    }

    @Override
    public List<WorkTopupDaoBalanceVo> selectTopUpBalanceByAddress(String address) {
        return workTopupHarvestMapper.selectTopUpBalanceByAddress(address);
    }

    @Override
    public List<UserTopupBalanceDetailsVo> selectTopUpSeeMore(String address, String daoId) {
        return workTopupHarvestMapper.selectTopUpSeeMore(address, daoId);
    }

    @Override
    public TogetherDaoMakerVo getTogetherDaoMakerVo(String projectId) {
        return workTopupHarvestMapper.getTogetherDaoMakerVo(projectId);
    }

    @Override
    public WorkTopupHarvest selectByProjectIdAndUserAddress(String projectId, String userAddress) {
        return workTopupHarvestMapper.selectByProjectIdAndUserAddress(projectId, userAddress);
    }

    @Override
    public List<TopupNftListVo> getTopupRewardNftList(String projectId) {
        // 每一个plan周期结束后，获取这个seed nodes下，有资产的nft
        // a绑定到了b上，资产分给了b
        return workTopupHarvestMapper.getTopupRewardNftList(projectId);
    }

    @Override
    public WorkTopupHarvest selectByProjectIdAndNft(String projectId, String erc721Address, String workNumber) {
        return workTopupHarvestMapper.selectByProjectIdAndNft(projectId, erc721Address, workNumber);
    }

    @Override
    public UserTopupBalanceVo selectSumOnChainTokenByProjectId(String projectId) {
        return workTopupHarvestMapper.selectSumOnChainTokenByProjectId(projectId);
    }

    @Override
    public Integer getTopupHoldersByProjectId(String projectId) {
        return workTopupHarvestMapper.getTopupHoldersByProjectId(projectId);
    }

    @Override
    public List<DaoProjectVo> selectPendingBalanceByAddress(String address) {
        return workTopupHarvestMapper.selectPendingBalanceByAddress(address);
    }

    @Override
    public List<UserTopupBalancePendingDetailVo> selectTopUpPendingSeeMore(String address, String projectId) {
        return workTopupHarvestMapper.selectTopUpPendingSeeMore(address, projectId);
    }

    @Override
    public List<TopupNftListVo> getTopupNftListVoByProjectAndAddress(String projectId, String address) {
        return workTopupHarvestMapper.getTopupNftListVoByProjectAndAddress(projectId, address);
    }

    @Override
    public WorkTopupHarvest selectOneByNft(String erc721Address, String workNumber) {
        return workTopupHarvestMapper.selectOneByNft(erc721Address, workNumber);
    }
}
