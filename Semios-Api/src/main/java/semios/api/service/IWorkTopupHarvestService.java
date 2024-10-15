package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.MakerInfoStatistics;
import semios.api.model.entity.WorkTopupHarvest;
import semios.api.model.vo.req.DaoProjectVo;
import semios.api.model.vo.res.*;
import semios.api.model.vo.res.TopUpReward.TopupNftListVo;

import java.util.List;

/**
 * <p>
 * topup模式work下的eth和token的数量 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-01-30
 */
public interface IWorkTopupHarvestService extends IService<WorkTopupHarvest> {
    WorkTopupHarvest selectByProjectIdAndMountWorkId(String projectId, Integer mountWorkId);

    List<WorkTopupHarvest> selectListByMountWorkIdAndMount721Address(Integer mountWorkId, String mount721Address);

    List<WorkTopupDaoBalanceVo> selectTopUpBalanceByAddress(String address);

    List<UserTopupBalanceDetailsVo> selectTopUpSeeMore(String address, String daoId);

    TogetherDaoMakerVo getTogetherDaoMakerVo(String projectId);

    WorkTopupHarvest selectByProjectIdAndUserAddress(String projectId, String userAddress);

    List<TopupNftListVo> getTopupRewardNftList(String projectId);

    WorkTopupHarvest selectByProjectIdAndNft(String projectId, String erc721Address, String workNumber);

    UserTopupBalanceVo selectSumOnChainTokenByProjectId(String projectId);

    Integer getTopupHoldersByProjectId(String projectId);

    List<DaoProjectVo> selectPendingBalanceByAddress(String address);

    List<UserTopupBalancePendingDetailVo> selectTopUpPendingSeeMore(String address, String projectId);

    List<TopupNftListVo> getTopupNftListVoByProjectAndAddress(String projectId, String address);

    WorkTopupHarvest selectOneByNft(String erc721Address, String workNumber);

    List<MakerInfoStatistics> selectAllMakerInfo();
}
