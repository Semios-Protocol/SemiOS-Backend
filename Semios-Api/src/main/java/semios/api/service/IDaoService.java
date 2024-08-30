package semios.api.service;

import java.math.BigDecimal;
import java.util.List;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import org.apache.ibatis.annotations.Param;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.response.NewProjectUriDto;
import semios.api.model.entity.*;
import semios.api.model.vo.req.DaoEditReqVo;
import semios.api.model.vo.req.DaoExportInfoParam.DaoExportParam;
import semios.api.model.vo.req.DaoIdReqVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.res.DaoExportInfo.DaoExportInfoVo;

/**
 * <p>
 * dao 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IDaoService extends IService<Dao> {

    Integer searchAmount(String searchId);

    Integer searchSeedNodesAmount(String searchId);

    List<Dao> searchDao(String searchId);

    Page<Dao> searchDao(IPage<Dao> page,String searchId);

    List<Dao> selectDaoByIds(List<Integer> ids);

    List<Dao> searchSeedNodes(String searchId);

    Dao daoDetailByDaoId(Integer daoId);

    Page<Dao> myDaoList(IPage<Dao> page, String ownerAddress);

    Page<Dao> findFavoritesByUserAddress(IPage<Dao> page, String userAddress);

    Page<DaoDrbStatistics> collectionsDao(IPage<DaoDrbStatistics> page, DaoSortedReqVo daoSortedReqVo);

    Page<Dao> exploreSeedNodes(IPage<DaoDrbStatistics> page, DaoSortedReqVo daoSortedReqVo);

    Dao daoDetailByProjectId(String projectId);

    List<Dao> daoDetailByProjectIdList(List<String> projectIdList);

    Dao daoDetailByUri(String daoUri);

    List<Dao> daoStarted();

    List<Dao> selectStartDaoByDrb(Integer drb);

    List<Dao> selectEndedDaoByDrb(Integer drb);

    Dao daoDetailByDaoName(String daoName);

    Dao selectDaoByErc721Token(String erc721Token);

    Dao selectDaoByErc20Token(String erc20Token);

    List<Dao> selectDaoListByErc20TokenNoPage(String erc20Token);

    int updateDaoPaused(Dao dao, ShutdownRecord shutdownRecord, List<Canvas> canvasList);

    List<Dao> myDaoListAll(String ownerAddress);

    Dao selectDaoByDaoNumber(Integer daoNumber);

    Dao selectSeedNodesByDaoNumber(Integer daoNumber);

    List<Dao> selectDaoByDaoNumberList(List<Integer> daoNumberList);

    List<Integer> selectNotAvailableDaoNumber(Integer daoNumber);

    int editDaoRefreshOpensea(boolean isChanged, Dao dao, NewProjectUriDto newProjectUriDto, DaoEditReqVo daoEditReqVo);

    List<Dao> freshDaoOpenseaApi();

    List<Dao> selectDaoByErc20TokenList(List<String> erc20TokenList);

    List<Dao> freshDaoSymbolAndErc20Name();

    List<Dao> syncDexForErc20();

    Dao selectDaoByTransactionHash(String transactionHash);

    Dao selectDaoBySplitterAddress(String splitterAddress);

    int updateDaoRoyaltyFeeIncome(int daoId, BigDecimal originValue, BigDecimal newValue);

    int updateDaoAndSubscribe(Dao dao, List<Subscribe> subscribeList);

    List<Dao> syncDaoStatus();

    List<Dao> selectGenerationMethodDao();

    List<Dao> protoDaoList();

    List<Dao> protoDaoGenerateList();

    Long selectTotalDaoTokenByErc20Token(String erc20Token);

    Page<Dao> selectDaoListByErc20Token(IPage<Dao> daoIPage, DaoSortedReqVo daoSortedReqVo);

    List<Dao> selectByExistDaoId(String existDaoId);

    Boolean selectTopupDaoByExistDaoId(String existDaoId);

    /**
     * 返回togetherDaoId
     *
     * @param togetherDao
     * @param updatedao
     * @return
     */
    int insertTogetherDao(Dao togetherDao, Dao updatedao);


    List<Dao> selectByTogetherDaoId(String togetherDaoId);

    Page<Dao> selectByTogetherDaoIdPage(IPage<Dao> daoIPage, String togetherDaoId);

    List<Dao> listAll();

    // 通过projectID 获取聚合dao
    Dao getDaoByProjectId(String projectId,Integer isTogetherDao);

    // 通过projectID 获取聚合dao
    Dao getTogetherDaoBySubDaoProjectId(String projectId);

    Page<Dao> getCollectionsDaoList(IPage<Dao> page, DaoSortedReqVo daoSortedReqVo);

    Page<Dao> getDaoListByTogetherDaoId(IPage<Dao> page, String togetherDaoId);

    // 导出dao信息
    Result<DaoExportInfoVo> daoExportInfo(DaoExportParam daoExportParam);
}
