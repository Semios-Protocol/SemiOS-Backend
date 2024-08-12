package semios.api.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.entity.Work;
import semios.api.model.vo.req.CanvasSortedReqVo;
import semios.api.model.vo.req.DaoIdParam;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.WorkId;
import semios.api.model.vo.res.BaseWorkVo.WorkNftDetailsVo;
import semios.api.model.vo.res.MineNftVo;

import java.util.List;

/**
 * <p>
 * work作品 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IWorkService extends IService<Work> {

    Integer searchAmount(String searchId);

    List<Work> searchWork(String searchId);

    Page<Work> selectNftByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectUnmintedWorkByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectDrbNftByProjectId(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    // Page<Work> selectNftByCanvasId(IPage<Work> page, CanvasSortedReqVo canvasSortedReqVo);
    //
    // Page<Work> selectUnmintedWorkByCanvasId(IPage<Work> page, CanvasSortedReqVo canvasSortedReqVo);
    //
    // Page<Work> selectDrbNftByCanvasId(IPage<Work> page, String canvasId, String drb);

    Page<Work> selectWorkByCanvasId(IPage<Work> page, CanvasSortedReqVo canvasSortedReqVo);

    Page<Work> findFavoritesByUserAddress(IPage<Work> page, String userAddress);

    Work selectWorkById(String workId);

    Work selectWorkByNumber(Integer daoId, String workNumber);

    Page<Work> unmintedWorks(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectNfts(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    Page<Work> selectDrbNfts(IPage<Work> page, DaoSortedReqVo daoSortedReqVo);

    Page<Work> rankingNfts(IPage<Work> page);

    Page<Work> findHoldByUserAddress(IPage<Work> page, String userAddress);

    Page<Work> findMintedByUserAddress(IPage<Work> page, String userAddress);

    Page<Work> findCreatorByUserAddress(IPage<Work> page, String userAddress);

    int deleteWorkByIds(List<String> workIds);

    Work selectWorkByUri(String workUri);

    Work selectWorkByHash(String workHash);

    Integer selectNftOwners(String daoId);

    Integer selectNftAmounts(String daoId);

    Integer selectWorkAmounts(String daoId);

    Integer selectNftOwnersByCanvasId(String canvasId);

    Integer selectNftAmountsByCanvasId(String canvasId);

    Integer selectWorkAmountsByCanvasId(String canvasId);

    Double selectNftMintedPriceByDrbAndCanId(String canvasId, String drb);

    List<Work> selectWorksByDaoIdAndStatus(String daoId, Integer status);

    List<Work> selectNftNearlyTwoHours(Long nearlyTime);

    Work selectByNumber(Long daoNumber, Long canvasNumber, Long workNumber);

    Work findHoldByAddressAndDaoId(String userAddress, Integer daoId);

    List<Work> selecWorkForAnalytics(DaoAnalyticsBo daoAnalyticsBo);

    List<Work> selecWorkForTopOwnersAmount(DaoAnalyticsBo daoAnalyticsBo);

    List<Work> selectWorksForCanvasPic(String canvasId);

    Integer selectNftOwnersByEndDate(String daoId, String endDate);

    List<Work> selecWorkForOwnersAmount(DaoAnalyticsBo daoAnalyticsBo);

    List<Work> selectDrbNftByCanvasId(String canvasId, String drb);

    int selectCountHoldByAddressAndDaoId(String userAddress, Integer daoId);

    int selectDrbNftCountByCanvasId(String canvasId, Integer drb);

    int selectDrbNftCountByDaoId(String daoId, Integer drb);

    int selectRangeNftCountByCanvasId(String canvasId, Integer fromDrb, Integer endDrb);

    int selectRangeNftCountByDaoId(String daoId, Integer fromDrb, Integer endDrb);

    List<Integer> selectDaoMemberWork(String userAddress);

    Work selectLastGenerateWork(Integer daoId);

    int selectCountMintByAddressAndDaoId(String userAddress, Integer daoId);

    int selectCountNftGenerateWork(Integer daoId);

    List<Work> selectTopupWorkForCal(Integer drb);

    WorkCountBo selectDrbNftOwnerCountByDaoId(String daoId, Integer drb);

    List<Work> selectDrbNftByDaoId(Integer daoId, Integer drb);

    List<Work> selectWorksByDaoIds(List<Integer> daoIds);

    List<Work> selectWorksIsLockStatus(String blockNumber);

    Page<MineNftVo> workMintTopUp(IPage<Work> page, String userAddress, String projectId);

    Page<MineNftVo> workMintNotTopUp(IPage<Work> page, DaoIdParam daoIdParam);

    Page<WorkNftDetailsVo> workDetailNft(IPage<Work> page, WorkId workId);

    Page<Work> searchWork(IPage<Work> page, String searchId);

    Work selectWorkByTransactionHash(String transactionHash);


    Integer selectNftAmountsExceptZeroNft(String daoId);

    int selectDrbNftCountByDaoIdExceptZeroNft(String daoId, Integer drb);
}
