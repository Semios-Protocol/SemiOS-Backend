package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.WorkMapper;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.vo.req.*;
import semios.api.model.vo.res.BaseWorkVo.WorkNftDetailsVo;
import semios.api.model.vo.res.MineNftVo;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * work作品 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class WorkServiceImpl extends ServiceImpl<WorkMapper, Work> implements IWorkService {

    @Autowired
    private WorkMapper workMapper;

    @Autowired
    private IDaoService daoService;

    @Override
    public Integer searchAmount(String searchId) {
        return workMapper.searchAmount(searchId);
    }

    @Override
    public List<Work> searchWork(String searchId) {
        return workMapper.searchWork(searchId);
    }

    @Override
    public Page<Work> selectNftByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo) {
        return workMapper.selectNftByProjectId(page, daoSortedReqVo);
    }

    @Override
    public Page<Work> selectUnmintedWorkByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo) {
        return workMapper.selectUnmintedWorkByProjectId(page, daoSortedReqVo);
    }

    @Override
    public Page<Work> selectDrbNftByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo) {
        return workMapper.selectDrbNftByProjectId(page, daoSortedReqVo);
    }

    // @Override
    // public Page<Work> selectNftByCanvasId(IPage<Work> page, CanvasSortedReqVo canvasSortedReqVo) {
    // return workMapper.selectNftByCanvasId(page,canvasSortedReqVo);
    // }
    //
    // @Override
    // public Page<Work> selectUnmintedWorkByCanvasId(IPage<Work> page, CanvasSortedReqVo canvasSortedReqVo) {
    // return workMapper.selectUnmintedWorkByCanvasId(page, canvasSortedReqVo);
    // }
    //
    // @Override
    // public Page<Work> selectDrbNftByCanvasId(IPage<Work> page, String canvasId, String drb) {
    // return workMapper.selectDrbNftByCanvasId(page, canvasId, drb);
    // }

    @Override
    public Page<Work> selectWorkByCanvasId(IPage<Work> page, CanvasSortedReqVo canvasSortedReqVo) {
        return workMapper.selectWorkByCanvasId(page, canvasSortedReqVo);
    }

    @Override
    public Page<Work> findFavoritesByUserAddress(IPage<Work> page, String userAddress) {
        return workMapper.findFavoritesByUserAddress(page, userAddress);
    }

    @Override
    public Work selectWorkById(String workId) {
        return workMapper.selectWorkById(workId);
    }

    @Override
    public Work selectWorkByNumber(Integer daoId, String workNumber) {
        return workMapper.selectWorkByNumber(daoId, workNumber);
    }

    @Override
    public Page<Work> unmintedWorks(IPage<Work> page, DaoSortedReqVo daoSortedReqVo) {
        return workMapper.unmintedWorks(page, daoSortedReqVo);
    }

    @Override
    public Page<Work> selectNfts(IPage<Work> page, DaoSortedReqVo daoSortedReqVo) {
        return workMapper.selectNfts(page, daoSortedReqVo);
    }

    @Override
    public Page<Work> selectDrbNfts(IPage<Work> page, DaoSortedReqVo daoSortedReqVo) {
        return workMapper.selectDrbNfts(page, daoSortedReqVo);
    }

    @Override
    public Page<Work> rankingNfts(IPage<Work> page) {
        return workMapper.rankingNfts(page);
    }

    @Override
    public Page<Work> findHoldByUserAddress(IPage<Work> page, String userAddress) {
        return workMapper.findHoldByUserAddress(page, userAddress);
    }

    @Override
    public Page<Work> findMintedByUserAddress(IPage<Work> page, String userAddress) {
        return workMapper.findMintedByUserAddress(page, userAddress);
    }

    @Override
    public Page<Work> findCreatorByUserAddress(IPage<Work> page, String userAddress) {
        return workMapper.findCreatorByUserAddress(page, userAddress);
    }

    @Override
    public int deleteWorkByIds(List<String> workIds) {
        return workMapper.deleteWorkByIds(workIds);
    }

    @Override
    public Work selectWorkByUri(String workUri) {
        return workMapper.selectWorkByUri(workUri);
    }

    @Override
    public Work selectWorkByHash(String workHash) {
        return workMapper.selectWorkByHash(workHash);
    }

    @Override
    public Integer selectNftOwners(String daoId) {
        return workMapper.selectNftOwners(daoId);
    }

    @Override
    public Integer selectNftAmounts(String daoId) {
        return workMapper.selectNftAmounts(daoId);
    }

    @Override
    public Integer selectWorkAmounts(String daoId) {
        return workMapper.selectWorkAmounts(daoId);
    }

    @Override
    public Integer selectNftOwnersByCanvasId(String canvasId) {
        return workMapper.selectNftOwnersByCanvasId(canvasId);
    }

    @Override
    public Integer selectNftAmountsByCanvasId(String canvasId) {
        return workMapper.selectNftAmountsByCanvasId(canvasId);
    }

    @Override
    public Integer selectWorkAmountsByCanvasId(String canvasId) {
        return workMapper.selectWorkAmountsByCanvasId(canvasId);
    }

    @Override
    public Double selectNftMintedPriceByDrbAndCanId(String canvasId, String drb) {
        return workMapper.selectNftMintedPriceByDrbAndCanId(canvasId, drb);
    }

    @Override
    public List<Work> selectWorksByDaoIdAndStatus(String daoId, Integer status) {
        return workMapper.selectWorksByDaoIdAndStatus(daoId, status);
    }

    @Override
    public List<Work> selectNftNearlyTwoHours(Long nearlyTime) {
        return workMapper.selectNftNearlyTwoHours(nearlyTime);
    }

    @Override
    public Work selectByNumber(Long daoNumber, Long canvasNumber, Long workNumber) {
        return workMapper.selectByNumber(daoNumber, canvasNumber, workNumber);
    }

    @Override
    public Work findHoldByAddressAndDaoId(String userAddress, Integer daoId) {
        return workMapper.findHoldByAddressAndDaoId(userAddress, daoId);
    }

    @Override
    public List<Work> selecWorkForAnalytics(DaoAnalyticsBo daoAnalyticsBo) {
        return workMapper.selecWorkForAnalytics(daoAnalyticsBo);
    }

    @Override
    public List<Work> selecWorkForTopOwnersAmount(DaoAnalyticsBo daoAnalyticsBo) {
        return workMapper.selecWorkForTopOwnersAmount(daoAnalyticsBo);
    }

    @Override
    public List<Work> selectWorksForCanvasPic(String canvasId) {
        return workMapper.selectWorksForCanvasPic(canvasId);
    }

    @Override
    public Integer selectNftOwnersByEndDate(String daoId, String endDate) {
        return workMapper.selectNftOwnersByEndDate(daoId, endDate);
    }

    @Override
    public List<Work> selecWorkForOwnersAmount(DaoAnalyticsBo daoAnalyticsBo) {
        return workMapper.selecWorkForOwnersAmount(daoAnalyticsBo);
    }

    /**
     * @param canvasId canvas表id，非链上id
     * @param drb      drb
     * @return
     */
    @Override
    public List<Work> selectDrbNftByCanvasId(String canvasId, String drb) {
        return workMapper.selectDrbNftByCanvasId(canvasId, drb);
    }

    @Override
    public int selectCountHoldByAddressAndDaoId(String userAddress, Integer daoId) {
        return workMapper.selectCountHoldByAddressAndDaoId(userAddress, daoId);
    }

    @Override
    public int selectDrbNftCountByCanvasId(String canvasId, Integer drb) {
        return workMapper.selectDrbNftCountByCanvasId(canvasId, drb);
    }

    @Override
    public int selectDrbNftCountByDaoId(String daoId, Integer drb) {
        return workMapper.selectDrbNftCountByDaoId(daoId, drb);
    }

    @Override
    public int selectRangeNftCountByCanvasId(String canvasId, Integer fromDrb, Integer endDrb) {
        return workMapper.selectRangeNftCountByCanvasId(canvasId, fromDrb, endDrb);
    }

    @Override
    public int selectRangeNftCountByDaoId(String daoId, Integer fromDrb, Integer endDrb) {
        return workMapper.selectRangeNftCountByDaoId(daoId, fromDrb, endDrb);
    }

    @Override
    public List<Integer> selectDaoMemberWork(String userAddress) {
        return workMapper.selectDaoMemberWork(userAddress);
    }

    @Override
    public Work selectLastGenerateWork(Integer daoId) {
        return workMapper.selectLastGenerateWork(daoId);
    }

    @Override
    public int selectCountMintByAddressAndDaoId(String userAddress, Integer daoId) {
        return workMapper.selectCountMintByAddressAndDaoId(userAddress, daoId);
    }

    @Override
    public int selectCountNftGenerateWork(Integer daoId) {
        return workMapper.selectCountNftGenerateWork(daoId);
    }

    @Override
    public List<Work> selectTopupWorkForCal(Integer drb) {
        return workMapper.selectTopupWorkForCal(drb);
    }

    @Override
    public WorkCountBo selectDrbNftOwnerCountByDaoId(String daoId, Integer drb) {
        return workMapper.selectDrbNftOwnerCountByDaoId(daoId, drb);
    }

    @Override
    public List<Work> selectDrbNftByDaoId(Integer daoId, Integer drb) {
        return workMapper.selectDrbNftByDaoId(daoId, drb);
    }

    @Override
    public List<Work> selectWorksByDaoIds(List<Integer> daoIds) {
        if (daoIds == null || daoIds.isEmpty()) {
            return new ArrayList<>();
        }
        return workMapper.selectWorksByDaoIds(daoIds);
    }

    @Override
    public List<Work> selectWorksIsLockStatus(String blockNumber) {
        if (StringUtils.isBlank(blockNumber)) {
            return new ArrayList<>();
        }
        return workMapper.selectWorksByLockStatus(blockNumber);
    }

    @Override
    public Page<MineNftVo> workMintTopUp(IPage<Work> page, String userAddress,String projectId) {
        return workMapper.selectWorkMintTopUp(page,userAddress,projectId);
    }

    @Override
    public Page<MineNftVo> workMintNotTopUp(IPage<Work> page, DaoIdParam daoIdParam) {
        Dao dao = daoService.getById(daoIdParam.getDaoId());
        if (dao == null) {
            return new Page<>(daoIdParam.getPageNo(), daoIdParam.getPageSize());
        }
        String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();

        return workMapper.selectWorkMintNotTopUp(page,daoIdParam.getUserAddress(),existDaoId);
    }

    @Override
    public Page<WorkNftDetailsVo> workDetailNft(IPage<Work> page, WorkId workId) {
        return workMapper.workDetailNft(page,workId.getWorkId());
    }

    @Override
    public Page<Work> searchWork(IPage<Work> page,String searchId){
        return workMapper.searchWork(page,searchId);
    }

    @Override
    public Work selectWorkByTransactionHash(String transactionHash) {
        return workMapper.selectWorkByTransactionHash(transactionHash);
    }
}
