package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.mapper.*;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.NewProjectUriDto;
import semios.api.model.entity.*;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.vo.req.DaoEditReqVo;
import semios.api.model.vo.req.DaoExportInfoParam.DaoExportParam;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.res.DaoExportInfo.DaoExportInfoVo;
import semios.api.model.vo.res.ExploreFilter.TokenType;
import semios.api.service.IDaoService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * dao 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Slf4j
@Service
public class DaoServiceImpl extends ServiceImpl<DaoMapper, Dao> implements IDaoService {

    @Autowired
    private DaoMapper daoMapper;

    @Autowired
    private SubscribeMapper subscribeMapper;

    @Autowired
    private CanvasMapper canvasMapper;

    @Autowired
    private ShutdownRecordMapper shutdownRecordMapper;

    @Autowired
    private WorkMapper workMapper;

    private static final RestTemplate restTemplate = new RestTemplate();

    @Autowired
    private S3Service s3Service;

    @Override
    public Integer searchAmount(String searchId) {
        return daoMapper.searchAmount(searchId);
    }

    @Override
    public Integer searchSeedNodesAmount(String searchId) {
        return daoMapper.searchSeedNodesAmount(searchId);
    }

    @Override
    public List<Dao> searchDao(String searchId) {
        return daoMapper.searchDao(searchId);
    }

    @Override
    public List<Dao> searchSeedNodes(String searchId) {
        return daoMapper.searchSeedNodes(searchId);
    }

    @Override
    public Dao daoDetailByDaoId(Integer daoId) {
        return daoMapper.daoDetailByDaoId(daoId);
    }

    @Override
    public Page<Dao> myDaoList(IPage<Dao> page, String ownerAddress) {
        return daoMapper.myDaoList(page, ownerAddress);
    }

    @Override
    public List<Dao> selectDaoByIds(List<Integer> ids) {
        return daoMapper.selectBatchIds(ids);
    }

    @Override
    public Page<Dao> findFavoritesByUserAddress(IPage<Dao> page, String userAddress) {
        return daoMapper.findFavoritesByUserAddress(page, userAddress);
    }

    @Override
    public Page<DaoDrbStatistics> collectionsDao(IPage<DaoDrbStatistics> page, DaoSortedReqVo daoSortedReqVo) {
        return daoMapper.collectionsDao(page, daoSortedReqVo);
    }

    @Override
    public Page<Dao> exploreSeedNodes(IPage<DaoDrbStatistics> page, DaoSortedReqVo daoSortedReqVo) {
        return daoMapper.exploreSeedNodes(page, daoSortedReqVo);
    }

    @Override
    public Dao daoDetailByProjectId(String projectId) {
        return daoMapper.daoDetailByProjectId(CommonUtil.removeHexPrefixIfExists(projectId));
    }

    @Override
    public List<Dao> daoDetailByProjectIdList(List<String> projectIdList) {
        if (projectIdList == null || projectIdList.size() == 0) {
            return new ArrayList<>();
        }
        return daoMapper.daoDetailByProjectIdList(projectIdList);
    }

    @Override
    public Dao daoDetailByUri(String daoUri) {
        return daoMapper.daoDetailByUri(daoUri);
    }

    @Override
    public List<Dao> daoStarted() {
        return daoMapper.daoStarted();
    }

    @Override
    public List<Dao> selectStartDaoByDrb(Integer drb) {
        return daoMapper.selectStartDaoByDrb(drb);
    }

    @Override
    public List<Dao> selectEndedDaoByDrb(Integer drb) {
        return daoMapper.selectEndedDaoByDrb(drb);
    }

    @Override
    public Dao daoDetailByDaoName(String daoName) {
        return daoMapper.daoDetailByDaoName(daoName);
    }

    @Override
    public Dao selectDaoByErc721Token(String erc721Token) {
        return daoMapper.selectDaoByErc721Token(erc721Token);
    }

    @Override
    public Dao selectDaoByErc20Token(String erc20Token) {
        return daoMapper.selectDaoByErc20Token(erc20Token);
    }

    @Override
    public List<Dao> selectDaoListByErc20TokenNoPage(String erc20Token) {
        return daoMapper.selectDaoListByErc20TokenNoPage(erc20Token);
    }

    @Override
    public Dao selectDaoByDaoNumber(Integer daoNumber) {
        return daoMapper.selectDaoByDaoNumber(daoNumber);
    }

    @Override
    public Dao selectSeedNodesByDaoNumber(Integer daoNumber) {
        return daoMapper.selectSeedNodesByDaoNumber(daoNumber);
    }

    @Override
    public List<Dao> selectDaoByDaoNumberList(List<Integer> daoNumberList) {
        if (daoNumberList == null || daoNumberList.size() == 0) {
            return new ArrayList<>();
        }
        return daoMapper.selectDaoByDaoNumberList(daoNumberList);
    }

    @Transactional
    @Override
    public int updateDaoPaused(Dao dao, ShutdownRecord shutdownRecord, List<Canvas> canvasList) {
        int i = daoMapper.updateById(dao);
        i += shutdownRecordMapper.insert(shutdownRecord);
        if (canvasList != null && canvasList.size() > 0) {
            for (Canvas canvas : canvasList) {
                i += canvasMapper.updateById(canvas);
            }
        }
        return i;
    }

    @Override
    public List<Dao> myDaoListAll(String ownerAddress) {
        return daoMapper.myDaoListAll(ownerAddress);
    }

    @Override
    public List<Integer> selectNotAvailableDaoNumber(Integer daoNumber) {
        return daoMapper.selectNotAvailableDaoNumber(daoNumber);
    }

    @Override
    @Transactional
    public int editDaoRefreshOpensea(boolean isChanged, Dao dao, NewProjectUriDto newProjectUriDto,
                                     DaoEditReqVo daoEditReqVo) {

        int i = 0;
        try {
            // 处理uri用
            // 查询uri获取NewProjectUriDto对象，然后修改后重新修改到s3
            if (isChanged && !TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao())) {

                dao.setFreshOpensea(1);
                i += daoMapper.updateById(dao);

                String fileName = dao.getDaoUri().substring(dao.getDaoUri().lastIndexOf("/") + 1);

                // s3Service.deleteObject(
                // Dao4ArtConstant.bucketName + Dao4ArtConstant.metaBucketName + Dao4ArtConstant.daoBucketName,
                // fileName);

                BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
                representaion.setObjectName(fileName);
                representaion.setText(JacksonUtil.obj2json(newProjectUriDto));
                s3Service.putObject(
                        ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName,
                        representaion);
                log.info("[editDaoRefreshOpensea] s3FileName:{}", dao.getDaoUri());

                // opensea的collection不支持刷新 不做更新
                // 刷新oponsea名称
//                String openseaUrl = String.format(ProtoDaoConstant.openseaApiDaoLink, dao.getErc721Token());
//                log.info("[editDaoRefreshOpensea]openseaUrl:{}", openseaUrl);
//                HttpHeaders headers = new HttpHeaders();
//                headers.set("user-agent", "Chrome/83.0.4103.116");
//                if (StringUtils.isNotBlank(ProtoDaoConstant.openseaApiKey)) {
//                    headers.set("X-API-KEY", ProtoDaoConstant.openseaApiKey);
//                }
//                HttpEntity<String> httpEntity = new HttpEntity<>(headers);
//
//                ResponseEntity<String> responseEntity =
//                        restTemplate.exchange(openseaUrl, HttpMethod.GET, httpEntity, String.class);
//                if (responseEntity.getStatusCode() != HttpStatus.OK) {
//                    log.error("[editDaoRefreshOpensea] refresh opensea error param:{} error:{}",
//                            JacksonUtil.obj2json(dao), responseEntity.getBody());
//                } else {
//                    dao.setFreshOpensea(0);
//                }
            }

        } catch (HttpClientErrorException httpClientErrorException) {
            log.warn("[editDaoRefreshOpensea] httpClientErrorException error param:{} e:{}", JacksonUtil.obj2json(dao),
                    httpClientErrorException);
            dao.setFreshOpensea(1);
        } catch (Exception e) {
            log.error("[editDaoRefreshOpensea] upload newProjectUriDto error param:{} e:{}", JacksonUtil.obj2json(dao),
                    e);
            throw new RuntimeException("network error please try again later.");
        }

        // 由于无法刷新opensea 暂时禁止修改
        // dao.setDaoName(daoEditReqVo.getDaoName());
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao()) && !dao.getDaoName().equals(daoEditReqVo.getDaoName())) {
            dao.setDaoName(daoEditReqVo.getDaoName());
        }
        dao.setDaoManitesto(daoEditReqVo.getDaoManitesto());
        dao.setDaoDescription(daoEditReqVo.getDaoDescription());
        if (StringUtils.isNotBlank(daoEditReqVo.getS3DaoLogoUrl())) {
            dao.setDaoLogoUrl(daoEditReqVo.getS3DaoLogoUrl());
        }
        if (StringUtils.isNotBlank(daoEditReqVo.getS3DaoBgBannerUrl())) {
            dao.setDaoBgBanner(daoEditReqVo.getS3DaoBgBannerUrl());
        }
        dao.setDiscordLink(daoEditReqVo.getDiscordLink());
        dao.setOpenseaLink(daoEditReqVo.getOpenseaLink());
        dao.setTwitterLink(daoEditReqVo.getTwitterLink());
        dao.setSocialLinks(daoEditReqVo.getSocialLinks());
        i += daoMapper.updateById(dao);


        // 修改该node下所有work 自动生成的图片..
        if (StringUtils.isNotBlank(daoEditReqVo.getOldDaoWorkUrl())) {
            workMapper.updateWorkUrl(dao.getId(), daoEditReqVo.getOldDaoWorkUrl(), dao.getDaoWorkUrl());// 修改用户上传的work
            workMapper.updatePassCardUrl(dao.getId(), dao.getDaoWorkUrl()); // 修改pass卡
        }

        return i;

    }

    @Override
    public List<Dao> freshDaoOpenseaApi() {
        return daoMapper.freshDaoOpenseaApi();
    }

    @Override
    public List<Dao> selectDaoByErc20TokenList(List<String> erc20TokenList) {
        return daoMapper.selectDaoByErc20TokenList(erc20TokenList);
    }

    @Override
    public List<Dao> freshDaoSymbolAndErc20Name() {
        return daoMapper.freshDaoSymbolAndErc20Name();
    }

    @Override
    public List<Dao> syncDexForErc20() {
        return daoMapper.syncDexForErc20();
    }

    @Override
    public Dao selectDaoByTransactionHash(String transactionHash) {
        return daoMapper.selectDaoByTransactionHash(transactionHash);
    }

    @Override
    public Dao selectDaoBySplitterAddress(String splitterAddress) {
        return daoMapper.selectDaoBySplitterAddress(splitterAddress);
    }

    @Override
    public int updateDaoRoyaltyFeeIncome(int daoId, BigDecimal originValue, BigDecimal newValue) {
        return daoMapper.updateDaoRoyaltyFeeIncome(daoId, originValue, newValue);
    }

    @Override
    @Transactional
    public int updateDaoAndSubscribe(Dao dao, List<Subscribe> subscribeList) {
        int i = daoMapper.updateById(dao);
        if (subscribeList != null && subscribeList.size() > 0) {
            for (Subscribe subscribe : subscribeList) {
                if (subscribe.getId() == null) {
                    i += subscribeMapper.insert(subscribe);
                } else {
                    i += subscribeMapper.updateById(subscribe);
                }

            }
        }
        return i;
    }

    @Override
    public List<Dao> syncDaoStatus() {
        return daoMapper.syncDaoStatus();
    }

    @Override
    public List<Dao> selectGenerationMethodDao() {
        return daoMapper.selectGenerationMethodDao();
    }

    @Override
    public List<Dao> protoDaoList() {
        return daoMapper.protoDaoList();
    }

    @Override
    public List<Dao> protoDaoGenerateList() {
        return daoMapper.protoDaoGenerateList();
    }

    @Override
    public Long selectTotalDaoTokenByErc20Token(String erc20Token) {
        return daoMapper.selectTotalDaoTokenByErc20Token(erc20Token);
    }

    @Override
    public Page<Dao> selectDaoListByErc20Token(IPage<Dao> daoIPage, DaoSortedReqVo daoSortedReqVo) {
        return daoMapper.selectDaoListByErc20Token(daoIPage, daoSortedReqVo);
    }

    @Override
    public List<Dao> selectByExistDaoId(String existDaoId) {
        return daoMapper.selectByExistDaoId(existDaoId);
    }

    @Override
    public Boolean selectTopupDaoByExistDaoId(String existDaoId) {
        return daoMapper.selectTopupDaoByExistDaoId(existDaoId);
    }

    @Transactional
    @Override
    public int insertTogetherDao(Dao togetherDao, Dao updatedao) {
        int daoId = 0;
        if (togetherDao != null) {
            daoId = daoMapper.insert(togetherDao);

            updatedao.setTogetherDaoId(togetherDao.getId());
        }

        daoMapper.updateById(updatedao);
        return daoId;
    }


    @Override
    public List<Dao> selectByTogetherDaoId(String togetherDaoId) {
        return daoMapper.selectByTogetherDaoId(togetherDaoId);
    }

    @Override
    public Page<Dao> selectByTogetherDaoIdPage(IPage<Dao> daoIPage, String togetherDaoId) {
        return daoMapper.selectByTogetherDaoIdPage(daoIPage, togetherDaoId);
    }

    @Override
    public List<Dao> listAll() {
        return daoMapper.listAll();
    }

    @Override
    public Dao getDaoByProjectId(String projectId, Integer isTogetherDao) {
        return daoMapper.getDaoByProjectId(projectId, isTogetherDao);
    }

    @Override
    public Dao getTogetherDaoBySubDaoProjectId(String projectId) {
        return daoMapper.getTogetherDaoBySubDaoProjectId(projectId);
    }


    @Override
    public Page<Dao> getCollectionsDaoList(IPage<Dao> page, DaoSortedReqVo daoSortedReqVo) {
        return daoMapper.selectCollectionDao(page, daoSortedReqVo);
    }

    @Override
    public Page<Dao> getDaoListByTogetherDaoId(IPage<Dao> page, String togetherDaoId) {
        return daoMapper.getDaoListByTogetherDaoId(page, togetherDaoId);
    }

    @Override
    public Page<Dao> searchDao(IPage<Dao> page, String searchId) {
        return daoMapper.searchDao(page, searchId);
    }


    @Override
    public Result<DaoExportInfoVo> daoExportInfo(DaoExportParam daoExportParam) {
        Result<DaoExportInfoVo> result = new Result<>();
        // 获取dao信息
        Dao dao = daoMapper.selectById(daoExportParam.getDaoId());
        if (dao == null || dao.getIsTogetherDao() == 1) {
            result.setResultDesc(ResultDesc.AUTH_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            return result;
        }
        DaoExportInfoVo daoExportInfoVo = DaoExportInfoVo.tranferDaoExportInfoVo(dao, daoExportParam);
        result.setData(daoExportInfoVo);
        return result;
    }

    @Override
    public Result<TokenType> selectTokenType(SearchReqVo searchReqVo) {
        Result<TokenType> result = new Result<>();
        List<String> tokenTypeList = daoMapper.selectTokenType(searchReqVo);
        TokenType tokenType = new TokenType();
        tokenType.setTokenTypeList(tokenTypeList);

        result.setData(tokenType);
        return result;
    }

    @Override
    public List<String> selectTokenTypeList(SearchReqVo searchReqVo) {
        return daoMapper.selectTokenTypeList(searchReqVo);
    }

}
