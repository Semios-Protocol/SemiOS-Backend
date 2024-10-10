package semios.api.controller;

import com.amazonaws.util.Md5Utils;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;
import org.web3j.crypto.Hash;
import semios.api.interceptor.S3Service;
import semios.api.model.annotation.RepeatSubmit;
import semios.api.model.dto.chain.*;
import semios.api.model.dto.common.*;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.response.*;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.model.vo.PageVo;
import semios.api.model.vo.req.*;
import semios.api.model.vo.req.DaoExportInfoParam.DaoExportParam;
import semios.api.model.vo.res.*;
import semios.api.model.vo.res.DaoExportInfo.DaoExportInfoVo;
import semios.api.model.vo.res.ExploreFilter.TokenType;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.*;
import semios.api.utils.merkle.LeafValue;
import semios.api.utils.merkle.MerkleTree;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * dao相关接口dao相关接口
 *
 * @author xiangbin
 * @order 100
 */
@Slf4j
@RestController
@RequestMapping("/dao")
public class DaoController {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkService workService;

    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;

    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;

    @Autowired
    private S3Service s3Service;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IFavoritesService favoritesService;

    @Autowired
    private IUserService userService;

    @Autowired
    private IDaoStrategyService daoStrategyService;

    @Autowired
    private IWhiteListService whiteListService;

    @Autowired(required = false)
    private ISubscriptionService subscriptionService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Autowired
    private CommonService commonService;

    private static final RestTemplate restTemplate = new RestTemplate();

    /**
     * 展示7日内交易总金额最高的三个DAO
     */
    @PostMapping(value = "/gallery")
    public Result<DaoListVo> galleryDao(HttpServletRequest request) {

        Result<DaoListVo> result = new Result<>();
        List<DaoListVo> daoListVoList = new ArrayList<>();

        // 1。计算7日内drb 包括今天的
        // 先实时获取，失败了再计算，如果有变化了就计算
        Integer currentDrb = Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND);
        Integer sevenDayDrb = currentDrb - ProtoDaoConstant.SEVEN_DAY;
        log.info("[dao-gallery]currentDrb:{} sevenDayDrb:{}", currentDrb, sevenDayDrb);
        // 2。查询drb内分配情况排序，取前三个
        List<DaoDrbStatistics> daoDrbStatistics = new ArrayList<>();
        if (StringUtils.isBlank(ProtoDaoConstant.galleryDao)) {
            daoDrbStatistics = daoDrbStatisticsService.selectGalleryDao(sevenDayDrb, currentDrb);
        }

        if (daoDrbStatistics.size() < 3) {
            List<String> daoIdList = new ArrayList<>();
            Page<DaoDrbStatistics> iPage = new Page<>(1, 3);
            DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
            if (StringUtils.isNotBlank(ProtoDaoConstant.galleryDao)) {
                daoIdList = Arrays.asList(ProtoDaoConstant.galleryDao.split(","));
                daoSortedReqVo.setDaoIdList(daoIdList);
            }
            Page<DaoDrbStatistics> daoDrbStatisticsPage = daoService.collectionsDao(iPage, daoSortedReqVo);
            if (daoDrbStatisticsPage.getRecords().size() > 0) {
                List<DaoDrbStatistics> daoDrbStatistics2 = daoDrbStatisticsPage.getRecords();
                List<Integer> ids =
                        daoDrbStatistics.stream().map(DaoDrbStatistics::getDaoId).collect(Collectors.toList());
                if (StringUtils.isBlank(ProtoDaoConstant.galleryDao)) {
                    for (DaoDrbStatistics drbStatistics : daoDrbStatistics2) {
                        if (daoDrbStatistics.size() < 3 && (ids.isEmpty() || !ids.contains(drbStatistics.getDaoId()))) {
                            daoDrbStatistics.add(drbStatistics);
                        }
                    }
                } else {
                    Map<Integer, DaoDrbStatistics> daoDrbStatisticsMap = daoDrbStatistics2.stream()
                            .collect(Collectors.toMap(DaoDrbStatistics::getDaoId, v -> v, (v1, v2) -> v2));
                    if (daoDrbStatistics.size() < 3) {
                        for (String daoId : daoIdList) {
                            daoDrbStatistics.add(daoDrbStatisticsMap.get(Integer.valueOf(daoId)));
                        }
                    }
                }
            }
        }
        if (daoDrbStatistics.size() > 3) {
            daoDrbStatistics = daoDrbStatistics.subList(0, 3);
        }
        if (daoDrbStatistics.size() > 0) {
            String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
            List<Integer> favoritesIds = null;
            if (StringUtils.isNotBlank(userAddress)) {
                List<Favorites> favoritesList =
                        favoritesService.findListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
                favoritesIds =
                        favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
            }
            for (DaoDrbStatistics daoDrbStatistic : daoDrbStatistics) {
                if (daoDrbStatistic.getDaoFloorPrice() == null) {
                    daoDrbStatistic = daoDrbStatisticsService.selectLastedDrbByDaoId(daoDrbStatistic.getDaoId());
                }
                if (favoritesIds != null && favoritesIds.size() > 0) {
                    daoDrbStatistic.setFavorited(favoritesIds.contains(daoDrbStatistic.getDaoId()));
                }
                daoListVoList.add(DaoListVo.transferDrbStatistics(daoDrbStatistic));
            }
            result.setDataList(daoListVoList);
        } else {
            result.setDataList(new ArrayList<>());
        }

        return result;

    }

    /**
     * dao详情页 version_1.5
     */
    @PostMapping(value = "/detail")
    public Result<DaoDetailVo> daoDetail(@RequestBody(required = false) DaoIdReqVo daoIdReqVo,
                                         HttpServletRequest request) {

        Result<DaoDetailVo> result = new Result<>();

        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());

        DaoDetailVo daoDetailVo = DaoDetailVo.transfer(dao, daoDrbStatistics);

        Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
        daoDetailVo.setMintWindow(Long.valueOf(daoDrbStatisticsPage.getTotal()).intValue());

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(userAddress)) {
            if (dao.getOwnerAddress().equalsIgnoreCase(userAddress)) {
                daoDetailVo.setModifiable(true);
            }

            Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(),
                    dao.getId() + "", userAddress);
            if (favorites != null) {
                daoDetailVo.setFavorited(true);
            }
        }
        if (StringUtils.isNotBlank(dao.getExistDaoId())) {
            Dao existDao = daoService.daoDetailByProjectId(dao.getExistDaoId());
            if (existDao != null) {
                daoDetailVo.setMainDaoId(existDao.getId());
                daoDetailVo.setMainDaoName(existDao.getDaoName());
                daoDetailVo.setMainDaoProjectId(existDao.getProjectId());
            }
            if (existDao != null && existDao.getOwnerAddress().equals(userAddress)) {
                daoDetailVo.setIsMainDaoCreator(true);
            }
        } else {
            daoDetailVo.setIsMainDaoCreator(daoDetailVo.getModifiable());
        }
        result.setData(daoDetailVo);
        return result;
    }

    /**
     * dao编辑时查询详情页 version_1.1
     */
    @PostMapping(value = "/edit/detail")
    public Result<DaoDetailVo> daoEditDetail(@RequestBody(required = false) DaoIdReqVo daoIdReqVo,
                                             HttpServletRequest request) {

        Result<DaoDetailVo> result = new Result<>();

        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

//        if (!dao.getOwnerAddress().equals(userAddress)) {
//            result.setResultDesc("You are not the owner, please check the connected wallet account.");
//            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
//            return result;
//        }

        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());

        DaoDetailVo daoDetailVo = DaoDetailVo.transfer(dao, daoDrbStatistics);

        if (checkMainDaoOwner(dao, userAddress)) {
            daoDetailVo.setIsMainDaoCreator(true);
        } else {
            daoDetailVo.setIsMainDaoCreator(daoDetailVo.getModifiable());
        }

        Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
        daoDetailVo.setMintWindow(Long.valueOf(daoDrbStatisticsPage.getTotal()).intValue());
        result.setData(daoDetailVo);
        return result;
    }

    /**
     * dao下的NFT列表
     */
    @PostMapping(value = "/nfts")
    public Result<WorkListVo> daoNfts(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                      HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();

        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setProjectId(dao.getProjectId());
        Page<Work> workPage = workService.selectNftByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVos =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 1.6.1 dao下的NFT列表(图片形式) P0
     */
    @PostMapping(value = "/nfts/v2")
    public Result<WorkListVoV2> daoNftsV2(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                          HttpServletRequest request) {
        Result<WorkListVoV2> result = new Result<>();

        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setProjectId(dao.getProjectId());
        Page<Work> workPage = workService.selectNftByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVos =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * dao下未铸造的work列表 1.6
     */
    @PostMapping(value = "/unmintedWorks")
    public Result<WorkListVo> daoUnmintedWorks(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                               HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();

        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        if (StringUtils.isBlank(dao.getProjectId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        daoSortedReqVo.setProjectId(dao.getProjectId());
        if (StringUtils.isBlank(daoSortedReqVo.getSortCondition())) {
            daoSortedReqVo.setSortCondition("0");
        }
        Page<Work> workPage = workService.selectUnmintedWorkByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVos =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * 1.6.1 dao下未铸造的work列表(图片形式)P0
     */
    @PostMapping(value = "/unmintedWorks/v2")
    public Result<WorkListVoV2> daoUnmintedWorksV2(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                   HttpServletRequest request) {
        Result<WorkListVoV2> result = new Result<>();

        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        if (StringUtils.isBlank(dao.getProjectId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        daoSortedReqVo.setProjectId(dao.getProjectId());
        if (StringUtils.isBlank(daoSortedReqVo.getSortCondition())) {
            daoSortedReqVo.setSortCondition("0");
        }
        Page<Work> workPage = workService.selectUnmintedWorkByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVos =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * dao当前drb下铸造的nft
     */
    @PostMapping(value = "/drbNfts")
    public Result<WorkListVo> daoDrbNfts(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                         HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();

        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        daoSortedReqVo.setCurrentDrb(Integer.valueOf(dao.getCurrentRound()));
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setProjectId(dao.getProjectId());
        Page<Work> workPage = workService.selectDrbNftByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVos =
                works.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 1.6.1 dao当前drb下铸造的nft(图片形式) P0
     */
    @PostMapping(value = "/drbNfts/v2")
    public Result<WorkListVoV2> daoDrbNftsV2(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                             HttpServletRequest request) {
        Result<WorkListVoV2> result = new Result<>();
        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        daoSortedReqVo.setCurrentDrb(Integer.valueOf(dao.getCurrentRound()));
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setProjectId(dao.getProjectId());
        Page<Work> workPage = workService.selectDrbNftByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            works.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVos =
                works.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(0);
        result.setPage(page);
        return result;

    }

    /**
     * dao下每个canvas的rewards情况
     */
    @PostMapping(value = "/rewards")
    public Result<DaoRewardsVo> daoRewards(@RequestBody(required = false) DaoReqVo daoReqVo) {

        Result<DaoRewardsVo> result = new Result<>();

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<CanvasDrbStatistics> iPage = new Page<>(daoReqVo.getPageNo(), daoReqVo.getPageSize());

        Page<CanvasDrbStatistics> canvasDrbStatisticsPage =
                canvasDrbStatisticsService.selectByProjectId(iPage, dao.getProjectId());

        List<CanvasDrbStatistics> canvasDrbStatistics = canvasDrbStatisticsPage.getRecords();
        List<DaoRewardsVo> daoRewardsVoList =
                canvasDrbStatistics.stream().map(v -> DaoRewardsVo.transfer(v, dao)).collect(Collectors.toList());

        result.setDataList(daoRewardsVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoReqVo.getPageNo());
        page.setPageSize(daoReqVo.getPageSize());
        page.setCount(canvasDrbStatisticsPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * dao下Activity
     */
    @PostMapping(value = "/activity")
    public Result<DaoActivityVo> daoActivity(@RequestBody(required = false) DaoReqVo daoReqVo) {

        Result<DaoActivityVo> result = new Result<>();

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<DaoDrbStatistics> iPage = new Page<>(daoReqVo.getPageNo(), daoReqVo.getPageSize());
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());

        List<DaoDrbStatistics> daoDrbStatistics = daoDrbStatisticsPage.getRecords();
        List<DaoActivityVo> daoActivityVoList =
                daoDrbStatistics.stream().map(DaoActivityVo::transfer).collect(Collectors.toList());
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
            daoActivityVoList.forEach(v -> v.setErc20PaymentMode(true));
        }
        result.setDataList(daoActivityVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoReqVo.getPageNo());
        page.setPageSize(daoReqVo.getPageSize());
        page.setCount(daoDrbStatisticsPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * collections下的dao列表
     */
    @PostMapping(value = "/collections")
    public Result<TogetherDaoListVo> daoCollections(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                    HttpServletRequest request) {
        Result<TogetherDaoListVo> result = new Result<>();

        Page<DaoDrbStatistics> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoService.collectionsDao(iPage, daoSortedReqVo);
        List<DaoDrbStatistics> daoDrbStatistics = daoDrbStatisticsPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            daoDrbStatistics.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getDaoId())));
        }
        List<DaoListVo> daoListVoList =
                daoDrbStatistics.stream().map(DaoListVo::transferDrbStatistics).collect(Collectors.toList());
        log.info("[DaoController]--原来的值:" + JacksonUtil.obj2json(daoListVoList));

        // 1.4.3更改返回值
        if (daoListVoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            List<Integer> daoIdList = daoListVoList.stream().map(DaoListVo::getDaoId).collect(Collectors.toList());

            Map<Integer, Dao> daoMap = daoService.selectDaoByIds(daoIdList)
                    .stream()
                    .collect(Collectors.toMap(Dao::getId, dao -> dao, (existing, replacement) -> existing, LinkedHashMap::new));

            List<TogetherDaoListVo> togetherDaoListVoList = daoIdList.stream()
                    .map(v -> TogetherDaoListVo.transferTogetherDaoListVo(daoMap.get(v), userAddress))
                    .collect(Collectors.toList());

            //List<Dao> daoList = daoService.selectDaoByIds(daoIdList);
            //log.info("整合拿到的daoList:"+JacksonUtil.obj2json(daoList));

            //List<TogetherDaoListVo> togetherDaoListVoList = daoList.stream().map(v-> TogetherDaoListVo.transferTogetherDaoListVo(v,userAddress)).collect(Collectors.toList());
//        List<TogetherDaoListVo> togetherDaoListVoList = new ArrayList<>();
//        for (DaoDrbStatistics drbStatistics:daoDrbStatistics){
//            List<Dao> daoList = daoService.selectByTogetherDaoId(drbStatistics.getDaoId() + "");
//            daoList = daoList.stream().filter(v -> DaoStatusEnum.STARTED.getStatus().equals(v.getDaoStatus())).collect(Collectors.toList());
//            togetherDaoListVoList.addAll(daoList.stream().sorted(Comparator.comparing(Dao::getDaoName)).map(this::transferTogetherDaoListVo).collect(Collectors.toList()));
//        }
            result.setDataList(togetherDaoListVoList);
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(daoDrbStatisticsPage.getTotal());
        result.setPage(page);

        return result;

    }


    /**
     * 1.12 explore下的seedNodes列表
     */
    @PostMapping(value = "/seedNodes")
    public Result<SeedNodesListVo> daoExploreSeedNodes(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                       HttpServletRequest request) {
        Result<SeedNodesListVo> result = new Result<>();
        Page<DaoDrbStatistics> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Dao> daoPage = daoService.exploreSeedNodes(iPage, daoSortedReqVo);
        List<Dao> daoList = daoPage.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.SEED_NODES_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && !favoritesIds.isEmpty()) {
            List<Integer> favoritesIds2 = favoritesIds;
            daoList.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }

        List<SeedNodesListVo> seedNodesListVo =
                daoList.stream().map(SeedNodesListVo::transfer).collect(Collectors.toList());

        result.setDataList(seedNodesListVo);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(daoPage.getTotal());
        result.setPage(page);

        return result;

    }

    /**
     * rankings下的dao列表
     */
    @PostMapping(value = "/rankings")
    public Result<DaoRankingVo> daoRankings(@RequestBody(required = false) PageVo pageVo, HttpServletRequest request) {

        Result<DaoRankingVo> result = new Result<>();
        Page<DaoDrbStatistics> iPage = new Page<>(pageVo.getPageNo(), pageVo.getPageSize());
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.daosRanking(iPage);
        List<DaoDrbStatistics> daoDrbStatistics = daoDrbStatisticsPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            daoDrbStatistics.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getDaoId())));
        }
        List<DaoRankingVo> daoListVoList =
                daoDrbStatistics.stream().map(DaoRankingVo::transfer).collect(Collectors.toList());
        result.setDataList(daoListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(pageVo.getPageNo());
        page.setPageSize(pageVo.getPageSize());
        page.setCount(daoDrbStatisticsPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 创建dao页面 根据dao信息换取上链信息
     */
    @PostMapping(value = "/create")
    @RepeatSubmit(key = "dao_create")
    public Result<DaoCreateResVo> createDao(HttpServletRequest request, DaoCreateReqVo daoCreateReqVo) {

        /*
          不需要判断名字是否重复，每个都生成不同的uri，让用户选择最终哪个dao上链
          1。生成uri文件，保存图片地址，
          2。上传图片到aws返回图片地址
         */
        Result<DaoCreateResVo> result = new Result<>();
        log.info("[dao-create] param:{}", JacksonUtil.obj2json(daoCreateReqVo));
        if (daoCreateReqVo == null || StringUtils.isBlank(daoCreateReqVo.getDaoName())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(useraddress)) {
            daoCreateReqVo.setUserAddress(useraddress);
        } else {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        String name = daoCreateReqVo.getDaoName().trim();
        String checkString = CommonUtil.nameCheck(name);
        if (StringUtils.isNotBlank(checkString)) {
            result.setResultDesc("The naming convention is: \"Name limited to 45 characters, alphabetic letters and digits only.\"");
            List<String> stringList = Arrays.asList(ProtoDaoConstant.nameCheckList.split(","));
            if (stringList.contains(name)) {
                result.setResultDesc("This name is not available.");
            }
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoName(daoCreateReqVo.getDaoName());
        if (dao != null) {
            result.setResultDesc("The name is already taken.");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        String s3FileName = "";
        String s3DaoLogoUrl = "";
        String s3DaoBgBannerUrl = "";
        // logo可以不传
        if (daoCreateReqVo.getDaoLogo() != null
                && StringUtils.isNotBlank(daoCreateReqVo.getDaoLogo().getOriginalFilename())) {
            try {
                MultipartFile multipartFile = daoCreateReqVo.getDaoLogo();
                // 文件上传前的名称 处理图片用
                String daoLogo = multipartFile.getOriginalFilename();
                String imageName = CodeUtil.generateCode('D');
                daoLogo = imageName + daoLogo.substring(daoLogo.lastIndexOf("."));
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, multipartFile,
                        imageName);

                String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                s3DaoLogoUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + daoLogo;
                log.info("[dao-create] s3DaoLogoUrl:{}", s3DaoLogoUrl);
            } catch (Exception e) {
                log.error("[createDao] upload daoLogo error param:{} e:{}", JacksonUtil.obj2json(daoCreateReqVo), e);
                result.setResultDesc("network error please try again later.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }

        }

        if (daoCreateReqVo.getDaoBgBanner() != null
                && StringUtils.isNotBlank(daoCreateReqVo.getDaoBgBanner().getOriginalFilename())) {
            try {
                MultipartFile multipartFile = daoCreateReqVo.getDaoBgBanner();
                // 文件上传前的名称 处理图片用
                String bgBanner = multipartFile.getOriginalFilename();
                String imageName = CodeUtil.generateCode('B');
                bgBanner = imageName + bgBanner.substring(bgBanner.lastIndexOf("."));
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, multipartFile,
                        imageName);
                String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                s3DaoBgBannerUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + bgBanner;
                log.info("[dao-create] s3DaoBgBannerUrl:{}", s3DaoBgBannerUrl);

            } catch (Exception e) {
                log.error("[createDao] upload daoBgBanner error param:{} e:{}", JacksonUtil.obj2json(daoCreateReqVo),
                        e);
                result.setResultDesc("network error please try again later.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }

        }

        try {
            // 处理uri用
            daoCreateReqVo.setDaoLogoUrl(s3DaoLogoUrl);
            daoCreateReqVo.setDaoBgBannerUrl(s3DaoBgBannerUrl);
            NewProjectUriDto newProjectUriDto = NewProjectUriDto.transfer(daoCreateReqVo, true);
            String sourceString = JacksonUtil.obj2json(newProjectUriDto);
            assert sourceString != null;
            byte[] sourceByte = sourceString.getBytes(StandardCharsets.UTF_8);

            long second = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            String fileName = Md5Utils.md5AsBase64(sourceByte) + String.valueOf(second).substring(5) + ".json";
            fileName = CommonUtil.replaceOperator(fileName);
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            newProjectUriDto
                    .setUri(urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName + "/" + fileName);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(newProjectUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName,
                    representaion);
            s3FileName = urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName + "/" + fileName;
            log.info("[dao-create] s3FileName:{}", s3FileName);

        } catch (Exception e) {
            log.error("[createDao] upload newProjectUriDto error param:{} e:{}", JacksonUtil.obj2json(daoCreateReqVo),
                    e);
            result.setResultDesc("network error please try again later.");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }
        DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate dateParam = LocalDate.parse(daoCreateReqVo.getDaoStartDate(), df);
        Period period = Period.between(LocalDate.now(), dateParam);
        int periodDays = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
        periodDays = periodDays >= 0 ? periodDays : 0;
        Integer mintWindow = MintWindowEnum.getValueByIndex(Integer.parseInt(daoCreateReqVo.getDaoMintWindow()));
        DaoCreateFeeEnum daoCreateFeeEnum =
                DaoCreateFeeEnum.getDaoCreateFeeEnumByIndex(Integer.parseInt(daoCreateReqVo.getDaoCreateFee()));
        assert daoCreateFeeEnum != null;

        if (StringUtils.isBlank(ProtoDaoConstant.CURRENT_ROUND)) {
            log.error("[dao-create] current_round is null");
            result.setResultDesc("system exception, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        DaoCreateResVo daoCreateResVo = new DaoCreateResVo();
        daoCreateResVo.setStartDrb(Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) + periodDays);
        daoCreateResVo.setMintableRounds(mintWindow);
        daoCreateResVo.setFloorPriceRank(Integer.valueOf(daoCreateReqVo.getDaoFloorPrice()));
        daoCreateResVo.setMaxNftRank(Integer.parseInt(daoCreateReqVo.getTotalNftCasting()));
//        daoCreateResVo.setRoyaltyFee(daoCreateFeeEnum.getActualValue() + ProtoDaoConstant.MINT_D4A_FEE_RATIO);
        daoCreateResVo.setProjectUri(s3FileName);

        log.info("[dao-create] return daoCreateResVo:{}", JacksonUtil.obj2json(daoCreateResVo));

        result.setData(daoCreateResVo);
        return result;
    }

    /**
     * 当前用户dao列表- 返回数据在dataList中
     * DaoNameListVo
     */
    @PostMapping(value = "/mydao")
    public Result<TogetherDaoListVo> myDao(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<TogetherDaoListVo> result = new Result<>();
        Page<Dao> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Dao> daoPage = daoService.myDaoList(iPage, userProfilePageReqVo.getUserAddress());
        List<Dao> daoList = daoPage.getRecords();
        if (daoList.size() > 0) {
            List<DaoNameListVo> daoNameListVos =
                    daoList.stream().filter(v -> !DaoStatusEnum.NOT_CREATED.getStatus().equals(v.getDaoStatus()))
                            .map(DaoNameListVo::transfer).collect(Collectors.toList());
            log.info("[myDao]--原来的值:" + JacksonUtil.obj2json(daoNameListVos));
            List<Integer> daoIdList = daoNameListVos.stream().map(DaoNameListVo::getDaoId).map(Integer::valueOf).collect(Collectors.toList());
            Map<Integer, Dao> daoMap = daoService.selectDaoByIds(daoIdList)
                    .stream()
                    .collect(Collectors.toMap(Dao::getId, dao -> dao, (existing, replacement) -> existing, LinkedHashMap::new));

            List<TogetherDaoListVo> togetherDaoListVoList = daoIdList.stream()
                    .map(v -> TogetherDaoListVo.transferTogetherDaoListVo(daoMap.get(v), userProfilePageReqVo.getUserAddress()))
                    .collect(Collectors.toList());
            //List<TogetherDaoListVo> togetherDaoListVoList = daoListTo.stream().map(v-> TogetherDaoListVo.transferTogetherDaoListVo(v,userProfilePageReqVo.getUserAddress())).collect(Collectors.toList());

            result.setDataList(togetherDaoListVoList);
        } else {
            result.setDataList(new ArrayList<>());
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(daoPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 修改dao信息 version_1.1
     */
    @PostMapping(value = "/edit")
    @RepeatSubmit(key = "dao_edit")
    public Result<String> editDao(DaoEditReqVo daoEditReqVo, HttpServletRequest request) {

        Result<String> result = new Result<>();
        boolean isChanged = false;

        if (daoEditReqVo == null) {
            // 由于无法刷新opensea 暂时禁止修改
            // if (daoEditReqVo == null || StringUtils.isBlank(daoEditReqVo.getDaoName())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(daoEditReqVo.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        //1.1改为dao结束后仍可编辑
//        if (DaoStatusEnum.FINISHED.getStatus() == dao.getDaoStatus()) {
//            result.setResultCode(ResultDesc.FAIL.getResultCode());
//            result.setResultDesc("You can no longer edit as the DAO Mint Window has ended.");
//            return result;
//        }
//        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

//        if (StringUtils.isBlank(userAddress) || !dao.getOwnerAddress().equals(userAddress)) {
//            result.setResultCode(ResultDesc.FAIL.getResultCode());
//            result.setResultDesc("You are not the owner, please check the connected wallet account.");
//            return result;
//        }
        NewProjectUriDto newProjectUriDto = null;
        if (!TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao())) {


            newProjectUriDto = restTemplate.getForObject(dao.getDaoUri(), NewProjectUriDto.class);

            // 由于无法刷新opensea 暂时禁止修改
            // if (!daoEditReqVo.getDaoName().equals(dao.getDaoName())) {
            // Dao daoName = daoService.daoDetailByDaoName(daoEditReqVo.getDaoName());
            // if (daoName != null) {
            // result.setResultDesc("Invalid name. The name is already taken.");
            // result.setResultCode(ResultDesc.ERROR.getResultCode());
            // return result;
            // }
            // isChanged = true;
            // newProjectUriDto.setName("DAO4Art " + daoEditReqVo.getDaoName());
            // }
            if (newProjectUriDto == null) {
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("system exception, please try again later!");
                return result;
            }

            if (!daoEditReqVo.getDaoManitesto().equals(dao.getDaoManitesto())) {
                isChanged = true;
                newProjectUriDto.setManitesto(daoEditReqVo.getDaoManitesto());
            }
            if (!daoEditReqVo.getDaoDescription().equals(dao.getDaoDescription())) {
                isChanged = true;
                newProjectUriDto.setDescription(daoEditReqVo.getDaoDescription());
            }

        }

        String s3DaoLogoUrl = "";
        String s3DaoBgBannerUrl = "";
        // logo可以不传
        if (daoEditReqVo.getDaoLogo() != null
                && StringUtils.isNotBlank(daoEditReqVo.getDaoLogo().getOriginalFilename())) {
            try {
                MultipartFile multipartFile = daoEditReqVo.getDaoLogo();

                // 文件上传前的名称 处理图片用
                String daoLogo = multipartFile.getOriginalFilename();
                String strSuffix = daoLogo.substring(daoLogo.lastIndexOf("."));
                String logoFilePath = String.format(ProtoDaoConstant.workImageDaoLogoUrl, dao.getDaoNumber()) + File.separatorChar + dao.getDaoNumber() + strSuffix;
                File logoFile = new File(logoFilePath);
                if (!logoFilePath.equals(logoFile.getPath())) {
                    log.info("[dao-create] logoFile.Path:{},logoFilePath:{}", logoFile.getPath(), logoFilePath);
                    result.setResultCode(ResultDesc.ERROR.getResultCode());
                    result.setResultDesc("path is error");
                    return result;
                }

                String filename = logoFile.getName();
                if (filename.contains("..") || filename.contains("/") || filename.contains("\\")) {
                    throw new IllegalArgumentException("Invalid filename");
                }

                FileUtils.copyInputStreamToFile(multipartFile.getInputStream(), logoFile);
//                multipartFile.transferTo(logoFile);
                String imageName = CodeUtil.generateCode('D') + strSuffix;

                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, logoFile, imageName, false);
                String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                s3DaoLogoUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + imageName;

                log.info("[dao-create] s3DaoLogoUrl:{}", s3DaoLogoUrl);
                // dao.setDaoLogoUrl(s3DaoLogoUrl);
                daoEditReqVo.setS3DaoLogoUrl(s3DaoLogoUrl);

                if (!TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao())) {
                    daoEditReqVo.setOldDaoWorkUrl(dao.getDaoWorkUrl()); // 存放之前的image workUrl

                    newProjectUriDto.setLogo(s3DaoLogoUrl);
                    //处理logo转work图片地址及高度／背景色 hash等信息 抽象为一个方法
                    String filePath = String.format(ProtoDaoConstant.workImageDaoLogoUrl, dao.getDaoNumber()) + File.separatorChar + "0" + strSuffix;
                    File imageFile = new File(filePath);
                    if (!filePath.equals(imageFile.getPath())) {
                        log.error("[dao-create] imageFile.Path:{},filePath:{}", imageFile.getPath(), filePath);
                        result.setResultCode(ResultDesc.ERROR.getResultCode());
                        result.setResultDesc("path is error");
                        return result;
                    }


                    filename = imageFile.getName();
                    if (filename.contains("..") || filename.contains("/") || filename.contains("\\")) {
                        throw new IllegalArgumentException("Invalid filename");
                    }

                    FileUtils.copyInputStreamToFile(multipartFile.getInputStream(), imageFile);
//                multipartFile.transferTo(imageFile);

                    // 修改名字，加入时间戳。防止被替换而导致已经Mint的work图片被替换
                    String daoLogoWorkName = dao.getDaoNumber() + System.currentTimeMillis() + "_0" + strSuffix;
                    ImageUtil.imageAddText(filePath, filePath, dao.getDaoName(), null, daoLogo.substring(daoLogo.lastIndexOf(".") + 1));
                    s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, imageFile,
                            daoLogoWorkName, false);

                    dao.setWorkUrlSuffix(strSuffix);
                    dao.setAddWork(0);
                    ImageUtil.HeightAndBgColor heightAndBgColor = ImageUtil.getImageRgb(imageFile);
                    if (heightAndBgColor != null) {
                        dao.setHeight(heightAndBgColor.getHeight());
                        dao.setColor(heightAndBgColor.getBgColor());
                    }


                    String daoWorkUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + daoLogoWorkName;
                    dao.setDaoWorkUrl(daoWorkUrl);
                    String workHash = ImageUtil.getMD5(imageFile);
                    dao.setWorkHash(workHash);

                    isChanged = true;
                }
            } catch (Exception e) {
                log.error("[createDao] upload daoLogo error param:{} e:{}", JacksonUtil.obj2json(daoEditReqVo), e);
                result.setResultDesc("network error please try again later.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }

        }

        if (!TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao())) {
            if (daoEditReqVo.getDaoBgBanner() != null
                    && StringUtils.isNotBlank(daoEditReqVo.getDaoBgBanner().getOriginalFilename())) {
                try {
                    MultipartFile multipartFile = daoEditReqVo.getDaoBgBanner();
                    // 文件上传前的名称 处理图片用
                    String bgBanner = multipartFile.getOriginalFilename();
                    String imageName = CodeUtil.generateCode('B');
                    bgBanner = imageName + bgBanner.substring(bgBanner.lastIndexOf("."));
                    s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, multipartFile,
                            imageName);
                    String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                    s3DaoBgBannerUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + bgBanner;
                    log.info("[dao-create] s3DaoBgBannerUrl:{}", s3DaoBgBannerUrl);
                    newProjectUriDto.setBg_banner(s3DaoBgBannerUrl);
                    // dao.setDaoBgBanner(s3DaoBgBannerUrl);
                    daoEditReqVo.setS3DaoBgBannerUrl(s3DaoBgBannerUrl);
                    isChanged = true;
                } catch (Exception e) {
                    log.error("[createDao] upload daoBgBanner error param:{} e:{}", JacksonUtil.obj2json(daoEditReqVo), e);
                    result.setResultDesc("network error please try again later.");
                    result.setResultCode(ResultDesc.ERROR.getResultCode());
                    return result;
                }

            }
        }


        int i = daoService.editDaoRefreshOpensea(isChanged, dao, newProjectUriDto, daoEditReqVo);
        log.info("[dao-edit] return i:{}", i);
        return result;
    }

    /**
     * 创建work时查询dao列表-只查询当前用户创建了canvas的dao 使用dataList数据
     */
    @PostMapping(value = "/list/createWork")
    public Result<DaoNameListVo> dao4CreateWork(@RequestBody(required = false) UserProfileReqVo userProfileReqVo) {

        Result<DaoNameListVo> result = new Result<>();
        Page<Canvas> iPage = new Page<>(1, 100);
        Page<Canvas> canvasPage = canvasService.myCanvas(iPage, userProfileReqVo.getUserAddress());
        if (iPage.getTotal() > 100) {
            iPage = new Page<>(1, iPage.getTotal());
            canvasPage = canvasService.myCanvas(iPage, userProfileReqVo.getUserAddress());
        }
        List<Canvas> canvasList = canvasPage.getRecords();
        if (canvasList == null || canvasList.size() == 0) {
            log.info("[dao4CreateWork] canvas size is zero");
            result.setDataList(new ArrayList<>());
            return result;
        }
        List<Integer> daoIds = canvasList.stream().map(Canvas::getDaoId).collect(Collectors.toList());

        List<Dao> daoList = daoService.listByIds(daoIds);

        List<DaoNameListVo> daoNameListVos = daoList.stream().map(DaoNameListVo::transfer).collect(Collectors.toList());
        result.setDataList(daoNameListVos);

        return result;
    }

    /**
     * 创建canvas查询 使用dataList数据
     */
    @PostMapping(value = "/list/createCanvas")
    public Result<DaoNameListVo> dao4CreateCanvas() {

        Result<DaoNameListVo> result = new Result<>();

        List<Dao> daoList = daoService.daoStarted();

        List<DaoNameListVo> daoNameListVos = daoList.stream().map(DaoNameListVo::transfer).collect(Collectors.toList());
        result.setDataList(daoNameListVos);

        return result;
    }

    /**
     * 名字校验方法
     *
     * @param nameCheckVo
     * @return
     */
    @PostMapping(value = "/name/check")
    public Result<Boolean> checkName(@RequestBody(required = false) NameCheckVo nameCheckVo) {
        Result<Boolean> result = new Result<>();
        /*
          DAO的名字不允许重复，Unminted、Unnamed、D4A、DAO4ART不可用
          连续的空格最终只保留一个，开头和结尾的空格删除,只允许英文字符
         */
        /*
         * Canvas Name不能超过45字符，Unminted、Unnamed、D4A、DAO4ART不可用
         * 同一个DAO下canvas name不可重复。连续的空格最终只保留一个，开头和结尾的空格删除,只允许英文字符
         */
        /*
         * User Name字数限制45字符不可重复，Unminted、Unnamed、D4A、DAO4ART不可用，连续的空格最终只保留一个，开头和结尾的空格删除,只允许英文字符 加数字
         */
        if (nameCheckVo == null || StringUtils.isAnyBlank(nameCheckVo.getType())) {
            result.setResultDesc("parameter is incorrect");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }
        String name = nameCheckVo.getName().trim();
        String checkString = CommonUtil.nameCheck(name);
        if (StringUtils.isNotBlank(checkString)) {
            result.setResultDesc("The naming convention is: \"Name limited to 45 characters, alphabetic letters and digits only.\"");
            List<String> stringList = Arrays.asList(ProtoDaoConstant.nameCheckList.split(","));
            if (stringList.contains(name)) {
                result.setResultDesc("This name is not available.");
            }
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        if (nameCheckVo.getType().equals(NameCheckTypeEnum.DAO.getType())) {// 0-dao 1-canvas 2-username
            Dao dao = daoService.daoDetailByDaoName(name);
            if (dao != null) {
                result.setResultDesc("The name is already taken.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }
        } else if (nameCheckVo.getType().equals(NameCheckTypeEnum.CANVAS.getType())) {// 0-dao 1-canvas 2-username

            if (StringUtils.isBlank(nameCheckVo.getDaoId())) {
                result.setResultDesc("please select dao first");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }
            Canvas canvas = canvasService.selectCanvasByName(name, nameCheckVo.getDaoId());
            if (canvas != null) {
                if (StringUtils.isBlank(nameCheckVo.getCanvasId())
                        || !canvas.getId().equals(Integer.valueOf(nameCheckVo.getCanvasId()))) {
                    result.setResultDesc("Invalid name. The name is already taken.");
                    result.setResultCode(ResultDesc.ERROR.getResultCode());
                    return result;
                }
            }
        } else if (nameCheckVo.getType().equals(NameCheckTypeEnum.USER_NAME.getType())) {// 0-dao 1-canvas 2-username
            User user = userService.findUserByName(name);
            if (user != null) {
                result.setResultDesc("Invalid name. The name is already taken.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            }
        }

        return result;
    }

    /**
     * 创建dao时选择的系统时间 1.6
     */
    @PostMapping(value = "/times")
    public Result<DaoTimesResDto> createDaoTimes() {

        DaoTimesResDto daoTimesResDto = new DaoTimesResDto();
        Result<DaoTimesResDto> result = new Result<>();

        if (ProtoDaoConstant.CREATE_PROJECT_FEE == null) {
            result.setResultDesc("system exception, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }
        long localTime1 = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
        long localTime = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);
        long currentTime = System.currentTimeMillis() / 1000;
        daoTimesResDto.setCurrentTime(currentTime);
        daoTimesResDto.setCreateProjectFee(new BigDecimal(String.valueOf(ProtoDaoConstant.CREATE_PROJECT_FEE))
                .divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 4, RoundingMode.FLOOR).doubleValue());
        log.info("[createDaoTimes] localTime:{} localTime1:{} currentTime:{}", localTime, localTime1, currentTime);
        result.setData(daoTimesResDto);

        return result;
    }

    /**
     * DAO下的canvas列表
     */
    @PostMapping(value = "/canvas")
    public Result<CanvasListResVo> canvasCollectioins(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                      HttpServletRequest request) {

        Result<CanvasListResVo> result = new Result<>();

        Page<CanvasDrbStatistics> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<CanvasDrbStatistics> canvasDrbStatisticsPage = canvasService.collectionsCanvas(iPage, daoSortedReqVo);
        List<CanvasDrbStatistics> canvasDrbStatistics = canvasDrbStatisticsPage.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = null;
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.CANVAS_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        if (favoritesIds != null && favoritesIds.size() > 0) {
            List<Integer> favoritesIds2 = favoritesIds;
            canvasDrbStatistics.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getCanId())));
        }
        List<CanvasListResVo> canvasListVoList = canvasDrbStatistics.stream().map(v -> {
            List<Work> workList = workService.selectWorksForCanvasPic(v.getCanvasId() + "");
            Canvas canvas = canvasService.getById(v.getCanvasId());
            return CanvasListResVo.transferDrbStatistics(v, canvas, workList);
        }).collect(Collectors.toList());

        result.setDataList(canvasListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(canvasDrbStatisticsPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 停机情况查询
     */
    @PostMapping(value = "/paused")
    public Result<D4APausedResVo> d4aPaused(@RequestBody(required = false) D4APausedReqVo d4APausedReqVo,
                                            HttpServletRequest request) {
        Result<D4APausedResVo> result = new Result<>();
        D4APausedResVo d4APausedResVo = new D4APausedResVo();

        d4APausedResVo.setD4aPaused(ProtoDaoConstant.D4APause ? 1 : 0);

        // DAO paused
        if (!StringUtils.isBlank(d4APausedReqVo.getDaoId())) {
            Dao dao = daoService.getById(d4APausedReqVo.getDaoId());
            if (dao != null) {
                d4APausedResVo.setDaoPaused(dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
            }

        }
        // canvas paused
        if (!StringUtils.isBlank(d4APausedReqVo.getCanvasId())) {
            Canvas canvas = canvasService.getById(d4APausedReqVo.getCanvasId());
            if (canvas != null) {
                d4APausedResVo
                        .setCanvasPaused(canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
                d4APausedResVo.setDaoPaused(canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
            }
        }

        result.setData(d4APausedResVo);
        return result;
    }

    /**
     * 查询所有可用的daoNumber
     */
    @PostMapping(value = "/available/number")
    public Result<DaoNumberListDto> daoAvailableNumber(@RequestBody(required = false) UserProfileReqVo userProfileReqVo,
                                                       HttpServletRequest request) {

        Result<DaoNumberListDto> result = new Result<>();

        String userAddress = userProfileReqVo.getUserAddress();
        log.info("[daoAvailableNumber] userAddress:{}", userAddress);

        DaoNumberListDto daoNumberListDto = new DaoNumberListDto();
        List<Integer> daoNumList = ProtoDaoConstant.daoNumberList;
        List<Integer> daoNumberList = daoService.selectNotAvailableDaoNumber(ProtoDaoConstant.DAO_RESERVE_NUMBER);
        if (daoNumberList.size() > 0) {
            daoNumList.removeAll(daoNumberList);
        }
        daoNumberListDto.setDaoNumberList(daoNumList);

        result.setData(daoNumberListDto);
        return result;
    }

    /**
     * 判断用户是否有创建预留dao的权限 data 返回0-没有权限 1-有权限
     */
    @PostMapping(value = "/new/available")
    public Result<Integer> newDaoAvailableUser(HttpServletRequest request) {

        Result<Integer> result = new Result<>();
        result.setData(0);

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        log.info("[newDaoAvailableUser] userAddress:{}", userAddress);

        if (StringUtils.isBlank(userAddress)) {
            return result;
        }
        User user = userService.findUserByAddressHash(userAddress);

        if (user == null || user.getRole() == null) {
            log.info("[newDaoAvailableUser] userAddress:{} user is null", userAddress);
            return result;
        }

        if (user.getRole().equals(1)) {
            result.setData(1);
        }

        return result;

    }

    /**
     * 判断当前用户是否有白名单权限 version_1.5
     */
    @PostMapping(value = "/user/authority")
    public Result<DaoAuthorityResDto> daoUserAuthority(@RequestBody DaoIdReqVo daoIdReqVo, HttpServletRequest request) {
        log.info("[daoUserAuthority] daoReqVo:{}", JacksonUtil.obj2json(daoIdReqVo));
        Result<DaoAuthorityResDto> result = new Result<>();
        DaoAuthorityResDto daoAuthorityResDto = new DaoAuthorityResDto();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

        log.info("[daoUserAuthority] userAddress:{}", userAddress);
        if (StringUtils.isBlank(userAddress)) {
            daoAuthorityResDto.setCreateCanvas(false);
            daoAuthorityResDto.setMintWork(false);
            result.setData(daoAuthorityResDto);
            return result;
        }
        if (daoIdReqVo == null || daoIdReqVo.getDaoId() == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao.getCanvasCreatedBlacklist() + dao.getCanvasCreatedWhitelist() + dao.getCanvasCreatedWhitelistNft() + dao.getMinterWorksBlacklist()
                + dao.getMinterWorksWhitelist() + dao.getMinterWorksWhitelistNft() + dao.getMintCap() + dao.getErc721MintCap() + dao.getErc721MintCapId() + dao.getGlobalMintCap() == 0) {
            result.setData(daoAuthorityResDto);
            return result;
        }

        daoAuthorityResDto.setCreateCanvas(checkCanvasCreateWhite(userAddress, dao));
        daoAuthorityResDto.setMintWork(checkMintWorkWhite(userAddress, dao));

        log.info("[daoUserAuthority] userAddress:{} daoAuthorityResDto:{}", userAddress,
                JacksonUtil.obj2json(daoAuthorityResDto));
        result.setData(daoAuthorityResDto);

        return result;

    }

    /**
     * 修改或保存dao白名单信息 version_1.5
     */
    @PostMapping(value = "/whitelist/proof")
    public Result<Boolean> daoWhiteListProof(@RequestBody DaoWhiteListReqVo daoWhiteListReqVo,
                                             HttpServletRequest request) {
        Result<Boolean> result = new Result<>();
        result.setData(true);

        List<WhiteList> whiteLists;
        try {
            log.info("[daoWhiteListProof] daoWhiteListReqVo:{}", JacksonUtil.obj2json(daoWhiteListReqVo));
            whiteLists = new ArrayList<>();
            if (StringUtils.isNotBlank(daoWhiteListReqVo.getCanvasCreateMerkleRoot())
                    && !ProtoDaoConstant.ZERO_MERKLE_ROOT.equals(daoWhiteListReqVo.getCanvasCreateMerkleRoot())) {
                WhiteList canvasCreateWhiteList = whiteListService.selectByAddressAndRoot(
                        daoWhiteListReqVo.getUserAddress(), daoWhiteListReqVo.getCanvasCreateMerkleRoot());
                if (canvasCreateWhiteList == null) {
                    canvasCreateWhiteList = new WhiteList();
                    canvasCreateWhiteList.setUserAddress(daoWhiteListReqVo.getUserAddress());
                    String originAddress = daoWhiteListReqVo.getCanvasCreateOriginAddress();
                    List<String> list = CommonUtil.checkAddress(originAddress);
                    MerkleTree mt = new MerkleTree(list);
                    mt.init();
                    String rootHash = mt.getRootHash();
                    if (!rootHash.equals(daoWhiteListReqVo.getCanvasCreateMerkleRoot().toLowerCase())) {
                        throw new RuntimeException("diff root hash, front:"
                                + daoWhiteListReqVo.getCanvasCreateMerkleRoot() + ", end:" + rootHash);
                    }
                    // canvasCreateWhiteList.setOriginAddress(daoWhiteListReqVo.getCanvasCreateOriginAddress());
                    canvasCreateWhiteList.setOriginAddress(JacksonUtil.obj2json(list));
                    // canvasCreateWhiteList.setProof(daoWhiteListReqVo.getCanvasCreateMerkleProof());
                    canvasCreateWhiteList.setProof(JacksonUtil.obj2json(mt));
                    canvasCreateWhiteList.setProofRootHash(rootHash);

                    whiteLists.add(canvasCreateWhiteList);
                }
            }

            if (StringUtils.isNotBlank(daoWhiteListReqVo.getMintingMerkleRoot())
                    && !ProtoDaoConstant.ZERO_MERKLE_ROOT.equals(daoWhiteListReqVo.getMintingMerkleRoot())
                    && !daoWhiteListReqVo.getMintingMerkleRoot().equals(daoWhiteListReqVo.getCanvasCreateMerkleRoot())) {
                WhiteList mintingWhiteList = whiteListService.selectByAddressAndRoot(daoWhiteListReqVo.getUserAddress(),
                        daoWhiteListReqVo.getMintingMerkleRoot());
                if (mintingWhiteList == null) {
                    mintingWhiteList = new WhiteList();
                    mintingWhiteList.setUserAddress(daoWhiteListReqVo.getUserAddress());
                    String originAddress = daoWhiteListReqVo.getMintingOriginAddress();
                    List<String> list = CommonUtil.checkAddress(originAddress);
                    MerkleTree mt = new MerkleTree(list);
                    mt.init();
                    String rootHash = mt.getRootHash();
                    if (!rootHash.equals(daoWhiteListReqVo.getMintingMerkleRoot().toLowerCase())) {
                        throw new RuntimeException(
                                "diff root hash, front:" + daoWhiteListReqVo.getMintingMerkleRoot() + ", end:" + rootHash);
                    }
                    // mintingWhiteList.setOriginAddress(daoWhiteListReqVo.getMintingOriginAddress());
                    mintingWhiteList.setOriginAddress(JacksonUtil.obj2json(list));
                    // mintingWhiteList.setProof(daoWhiteListReqVo.getMintingMerkleProof());
                    mintingWhiteList.setProof(JacksonUtil.obj2json(mt));
                    mintingWhiteList.setProofRootHash(daoWhiteListReqVo.getMintingMerkleRoot());

                    whiteLists.add(mintingWhiteList);
                }
            }
            if (whiteLists.size() > 0) {
                whiteListService.saveBatch(whiteLists);
            }
        } catch (RuntimeException runtimeException) {
            log.error("[dao-whitelist-proof]RuntimeException daoWhiteListReqVo:{} e:",
                    JacksonUtil.obj2json(daoWhiteListReqVo), runtimeException);
            result.setResultDesc(runtimeException.getMessage());
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setData(false);
        } catch (Exception e) {
            log.error("[dao-whitelist-proof]Exception daoWhiteListReqVo:{} e:", JacksonUtil.obj2json(daoWhiteListReqVo),
                    e);
            result.setResultDesc("system error, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setData(false);
        }
        return result;
    }

    /**
     * 1.7 修改接口--查询dao黑白名单信息 version_1.1
     */
    @PostMapping(value = "/blackandwhite/list")
    public Result<DaoWhiteListResVo> daoBlackAndWhiteList(@RequestBody DaoIdReqVo daoIdReqVo,
                                                          HttpServletRequest request) {
        Result<DaoWhiteListResVo> result = new Result<>();
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        DaoWhiteListResVo daoWhiteListResVo = new DaoWhiteListResVo();
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "DAO is not exist!");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        String userAddress = daoIdReqVo.getUserAddress();
//        if (StringUtils.isBlank(userAddress) || !dao.getOwnerAddress().equals(userAddress)) {
//            if (!checkMainDaoOwner(dao, userAddress)) {
//                result.setResultDesc("You are not the owner, please check the connected wallet account.");
//                result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
//                return result;
//            }
//        }
        daoWhiteListResVo.setDaoId(dao.getId());
        daoWhiteListResVo.setBasicDao(dao.getBasicDao());
        daoWhiteListResVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoWhiteListResVo.setDaoStatus(dao.getDaoStatus());
        daoWhiteListResVo.setDaoVersion(dao.getDaoVersion());
        daoWhiteListResVo.setCreateCanvas(daoWhiteListResVo.new Strategy());
        daoWhiteListResVo.setMinting(daoWhiteListResVo.new Strategy());
        if (dao.getCanvasCreatedBlacklist() + dao.getCanvasCreatedWhitelist() + dao.getCanvasCreatedWhitelistNft() + dao.getMinterWorksBlacklist()
                + dao.getMinterWorksWhitelist() + dao.getMinterWorksWhitelistNft() + dao.getMintCap() + dao.getErc721MintCap() + dao.getErc721MintCapId() + dao.getGlobalMintCap() == 0) {
            if (dao.getMintCap() == null || dao.getMintCap() == 0) {
                if (dao.getGlobalMintCap() != null) {
                    DaoWhiteListResVo.Strategy minting = daoWhiteListResVo.new Strategy();
                    minting.setMaxMintingAmount(dao.getGlobalMintCap());
                    daoWhiteListResVo.setMinting(minting);
                }
                result.setData(daoWhiteListResVo);
                return result;
            }
        }

        daoWhiteListResVo.setCreateCanvas(buildCanvasCreateWhite(dao, daoWhiteListResVo));
        daoWhiteListResVo.setMinting(buildMintWorkWhite(dao, daoWhiteListResVo));

        result.setData(daoWhiteListResVo);
        return result;
    }

    /**
     * 查询白名单merkle信息 version_1.5
     */
    @PostMapping(value = "/blackandwhite/merkle")
    public Result<DaoWhiteMerkelResVo> daoBlackAndWhiteMerkle(@RequestBody DaoIdReqVo daoIdReqVo,
                                                              HttpServletRequest request) {

        Result<DaoWhiteMerkelResVo> result = new Result<>();
        log.info("[dao-blackandwhite-merkle]daoIdReqVo:{}", JacksonUtil.obj2json(daoIdReqVo));
        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId()) || daoIdReqVo.getType() == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        DaoWhiteMerkelResVo daoWhiteMerkelResVo = new DaoWhiteMerkelResVo();
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "DAO is not exist!");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        try {
            DaoStrategy daoStrategy = null;
            if (daoIdReqVo.getType() == 1 && dao.getCanvasCreatedWhitelist() != 0) {// create canvas
                daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
            } else if (daoIdReqVo.getType() == 2 && dao.getMinterWorksWhitelist() != 0) {// minting
                daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
            }
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())
                    && Arrays.asList(daoStrategy.getOriginAddress().split(",")).contains(daoIdReqVo.getUserAddress())) {

                // 查询proof表
                WhiteList whiteList = whiteListService.getById(daoStrategy.getProofId());
                if (whiteList != null && StringUtils.isNotBlank(whiteList.getProof())) {
                    // List<Map> merkelTreeDtoList = JacksonUtil.json2list(whiteList1.getProof(), Map.class);
                    // Optional<Map> optionalMap = merkelTreeDtoList.stream()
                    // .filter(v -> v.get(daoIdReqVo.getUserAddress()) != null).findFirst();
                    // if (optionalMap.isPresent()) {
                    // String proofList = optionalMap.get().get(daoIdReqVo.getUserAddress()).toString();
                    // proofList = proofList.substring(1, proofList.length() - 1);
                    // if (StringUtils.isNotBlank(proofList)) {
                    // daoWhiteMerkelResVo.setProof(Arrays.asList(proofList.replaceAll(" ", "").split(",")));
                    // }
                    // daoWhiteMerkelResVo.setRootHash(whiteList1.getProofRootHash());
                    // }
                    MerkleTree mt = JacksonUtil.json2pojo(whiteList.getProof(), MerkleTree.class);
                    for (LeafValue leaf : mt.getValues()) {
                        if (leaf.getValue().equals(daoIdReqVo.getUserAddress())) {
                            daoWhiteMerkelResVo.setProof(MerkleTree.getProof(mt.getTree(), leaf.getTreeIndex()));
                            daoWhiteMerkelResVo.setRootHash(whiteList.getProofRootHash());
                            break;
                        }
                    }
                }
            }
        } catch (Exception e) {

            log.error("[dao-blackandwhite-merkle]error daoIdReqVo:{} e:", JacksonUtil.obj2json(daoIdReqVo), e);
            result.setResultDesc("system error, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
        }

        result.setData(daoWhiteMerkelResVo);

        return result;
    }

    /**
     * 1.4 修改链上参数
     */
    @PostMapping(value = "/benefits/distribute")
    public Result<DaoBenefitsDistributeResVo> daoBenefitsDistribute(@RequestBody DaoReqVo daoReqVo,
                                                                    HttpServletRequest request) {
        Result<DaoBenefitsDistributeResVo> result = new Result<>();
        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
//        if (StringUtils.isBlank(userAddress) || !dao.getOwnerAddress().equals(userAddress)) {
//            if (!checkMainDaoOwner(dao, userAddress)) {
//                result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
//                result.setResultDesc("You are not the owner, please check the connected wallet account.");
//                return result;
//            }
//        }


        Long totalErc20Token = daoService.selectTotalDaoTokenByErc20Token(dao.getErc20Token());

        // 不支持修改的情况
        DaoBenefitsDistributeResVo daoBenefitsDistributeResVo = new DaoBenefitsDistributeResVo();
        List<DaoAllocationVo> daoTokenAllocationVos = new ArrayList<>();
        List<DaoAllocationVo> daoEthAllocationVos = new ArrayList<>();
        List<BigDecimal> ethAllocation = new ArrayList<>();
        List<BigDecimal> daoAllocation = new ArrayList<>();
        daoTokenAllocationVos.add(new DaoAllocationVo(dao.getProjectId()));
        daoEthAllocationVos.add(new DaoAllocationVo(dao.getProjectId()));

        daoBenefitsDistributeResVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoBenefitsDistributeResVo.setDaoVersion(dao.getDaoVersion());
        daoBenefitsDistributeResVo.setDailyMintCap(dao.getDailyMintCap());
        //10亿写在常量中了
        daoBenefitsDistributeResVo.setDaoTokenAllocation(new BigDecimal(ProtoDaoConstant.ERC20_TOTAL).subtract(new BigDecimal(String.valueOf(totalErc20Token))));
        daoBenefitsDistributeResVo.setBasicDao(dao.getBasicDao());
        daoBenefitsDistributeResVo.setDaoStatus(dao.getDaoStatus());
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            daoBenefitsDistributeResVo.setUnifiedPrice(dao.getGlobalDaoPrice());
            daoBenefitsDistributeResVo.setUnifiedPriceSet(true);
        }
        //总代币数量减去所有已发放的代币数量
        if (StringUtils.isNotBlank(dao.getErc20TotalSupply())) {
            if (dao.getDaoReward() == null) {
                daoBenefitsDistributeResVo.setDaoTokenLeave(new BigDecimal(dao.getErc20TotalSupply()));
            } else {
                daoBenefitsDistributeResVo.setDaoTokenLeave(new BigDecimal(dao.getErc20TotalSupply()).subtract(dao.getDaoReward()));
            }
        }
        if (dao.getDaoVersion() >= 1) {
            DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
            if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
                daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
            }
            DaoReserveRatio fixedReserveRatio = new DaoReserveRatio(true);
            DaoReserveRatio unFixedReserveRatio = new DaoReserveRatio(false);
            if (StringUtils.isNotBlank(dao.getFixedReserveRatio())) {
                fixedReserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
            }
            if (StringUtils.isNotBlank(dao.getUnfixedReserveRatio())) {
                unFixedReserveRatio = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
            }
            if (dao.getDaoVersion() <= 2) {
                daoRoyaltyToken.setCanvasReward(
                        ProtoDaoCommonUtil.bigdecimalPercentageToString(daoRoyaltyToken.getCanvasReward(), true));
                daoRoyaltyToken.setMinterReward(
                        ProtoDaoCommonUtil.bigdecimalPercentageToString(daoRoyaltyToken.getMinterReward(), false));
            } else {
                daoRoyaltyToken.setCanvasReward(daoRoyaltyToken.getCanvasReward());
                daoRoyaltyToken.setMinterReward(daoRoyaltyToken.getMinterReward());
            }

            daoBenefitsDistributeResVo.setDaoRoyaltyToken(daoRoyaltyToken);
            daoBenefitsDistributeResVo.setFixedReserveRatio(fixedReserveRatio);
            daoBenefitsDistributeResVo.setUnFixedReserveRatio(unFixedReserveRatio);
            if (dao.getDaoVersion() > 2) {
                DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
                if (daoDrbStatistics != null && daoDrbStatistics.getNft() != null) {
                    daoBenefitsDistributeResVo.setNftNumber(Integer.valueOf(daoDrbStatistics.getNft()));
                }
                Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
                Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
                daoBenefitsDistributeResVo.setMintWindow(daoDrbStatisticsPage.getTotal());
                daoBenefitsDistributeResVo.setDaoMintWindow(MintWindowEnum.getIndexByValue(dao.getDaoMintWindow()));
//                daoBenefitsDistributeResVo.setDaoFloorPrice(NewDaoFloorPriceEnum.getValueByLable(dao.getDaoFloorPrice().doubleValue()));
                daoBenefitsDistributeResVo.setDaoFloorPrice(dao.getDaoFloorPrice());
                daoBenefitsDistributeResVo.setCanvasPriceFluctuationMethod(dao.getCanvasPriceFluctuationMethod());
                daoBenefitsDistributeResVo.setFluctuationMethodFactor(dao.getFluctuationMethodFactor());
                daoBenefitsDistributeResVo.setTotalNftMintCap(TotalNftMintCapEnum.getValueByLable(dao.getTotalNftCasting()));
            }
            //1.3 eth分配比例
            DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
            if (StringUtils.isNotBlank(dao.getEthRoyaltyToken())) {
                daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);
            }
            daoBenefitsDistributeResVo.setDaoEthRoyaltyToken(daoEthRoyaltyToken);

            //1.3 Asset Allocation Settings
//            String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
            List<DaoAllocationStrategy> daoAllocationStrategyList = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
            daoTokenAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
            daoEthAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());

            daoAllocation = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() != 0).sorted(Comparator.comparing(DaoAllocationStrategy::getRoyaltyType)).map(DaoAllocationStrategy::getRoyaltyProportion).collect(Collectors.toList());
            ethAllocation = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() != 0).sorted(Comparator.comparing(DaoAllocationStrategy::getRoyaltyType)).map(DaoAllocationStrategy::getRoyaltyProportion).collect(Collectors.toList());

        }

        daoBenefitsDistributeResVo.setDaoTokenAllocationVos(daoTokenAllocationVos);
        daoBenefitsDistributeResVo.setDaoEthAllocationVos(daoEthAllocationVos);
        daoBenefitsDistributeResVo.setDaoAllocation(daoAllocation);
        daoBenefitsDistributeResVo.setEthAllocation(ethAllocation);

        if (daoBenefitsDistributeResVo.getDaoRoyaltyToken() != null) {
            daoBenefitsDistributeResVo.setMinterReward(
                    daoBenefitsDistributeResVo.getDaoRoyaltyToken().getMinterReward().stripTrailingZeros().toPlainString());
            daoBenefitsDistributeResVo.setCanvasReward(
                    daoBenefitsDistributeResVo.getDaoRoyaltyToken().getCanvasReward().stripTrailingZeros().toPlainString());
        }
        if (daoBenefitsDistributeResVo.getUnFixedReserveRatio() != null) {
            daoBenefitsDistributeResVo.setDaoMintFee(ProtoDaoCommonUtil
                    .bigdecimalPercentageToString(daoBenefitsDistributeResVo.getUnFixedReserveRatio().getDaoMintFee()));
            daoBenefitsDistributeResVo.setCanvasMintFee(ProtoDaoCommonUtil
                    .bigdecimalPercentageToString(daoBenefitsDistributeResVo.getUnFixedReserveRatio().getCanvasMintFee()));
        }

        daoBenefitsDistributeResVo.setRoyaltyTokenLotteryMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode()));
        daoBenefitsDistributeResVo.setIsThirdpartyToken(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsThirdpartyToken()));
        daoBenefitsDistributeResVo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));

        daoBenefitsDistributeResVo.setInfiniteMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getInfiniteMode()));
        daoBenefitsDistributeResVo.setRemainingMintWindow(dao.getRemainingMintWindow());

        daoBenefitsDistributeResVo.setDaoTokenMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));

        daoBenefitsDistributeResVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoBenefitsDistributeResVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoBenefitsDistributeResVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoBenefitsDistributeResVo.setErc20TokenDecimals(dao.getErc20TokenDecimals() == null ? 18 : dao.getErc20TokenDecimals());


        daoBenefitsDistributeResVo.setErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();

        Dao mainDao = daoService.daoDetailByProjectId(projectId);
        if (mainDao != null && userAddress.equals(mainDao.getOwnerAddress())) {
            daoBenefitsDistributeResVo.setIsMainDaoCreator(true);
        }
        if (mainDao != null) {
            daoBenefitsDistributeResVo.setErc20Address(CommonUtil.addHexPrefixIfNotExist(mainDao.getErc20Token()));
        }

        result.setData(daoBenefitsDistributeResVo);
        return result;
    }

    /**
     * 1.4 创建protodao页面 根据dao信息换取上链信息 version_2.0
     */
    @PostMapping(value = "/basic/create")
    @RepeatSubmit(key = "basic_dao_create")
    public Result<BasicDaoCreateResVo> createBasicDao(HttpServletRequest request, @RequestBody(required = false) BasicDaoCreateReqVo bacisDaoCreateReqVo) {


        Result<BasicDaoCreateResVo> result = new Result<>();
        log.info("[createBasicDao] param:{}", JacksonUtil.obj2json(bacisDaoCreateReqVo));
        if (bacisDaoCreateReqVo == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        if (StringUtils.isBlank(bacisDaoCreateReqVo.getDaoName())) {
            result.setResultDesc("The naming convention is: \"Name limited to 45 characters, alphabetic letters and digits only.\"");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(useraddress)) {
            bacisDaoCreateReqVo.setUserAddress(useraddress);
        } else {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        String name = bacisDaoCreateReqVo.getDaoName().trim();
        String checkString = CommonUtil.nameCheck(name);
        if (StringUtils.isNotBlank(checkString)) {
            result.setResultDesc("The naming convention is: \"Name limited to 45 characters, alphabetic letters and digits only.\"");
            List<String> stringList = Arrays.asList(ProtoDaoConstant.nameCheckList.split(","));
            if (stringList.contains(name)) {
                result.setResultDesc("This name is not available.");
            }
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoName(bacisDaoCreateReqVo.getDaoName());
        if (dao != null) {
            result.setResultDesc("The name is already taken.");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        // test 环境不用做限制
        if (!EnvEnum.TEST.getEnv().equals(ProtoDaoConstant.activity)) {
            String isExistNodeName = NodeNameMapUtil.get(bacisDaoCreateReqVo.getDaoName());
            if (StringUtils.isNotBlank(isExistNodeName)) {
                result.setResultDesc("This name is being used by someone else. Please wait for 3 minutes.");
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                return result;
            } else {
                NodeNameMapUtil.put(bacisDaoCreateReqVo.getDaoName(), useraddress);
            }
        }


        String s3FileName;
        String daoUriHash;
        try {
            // 处理uri
            NewProjectUriDto newProjectUriDto = NewProjectUriDto.transfer(bacisDaoCreateReqVo);
            String sourceString = JacksonUtil.obj2json(newProjectUriDto);
            assert sourceString != null;
            byte[] sourceByte = sourceString.getBytes(StandardCharsets.UTF_8);

            long second = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            daoUriHash = Md5Utils.md5AsBase64(sourceByte) + String.valueOf(second).substring(5);
            daoUriHash = CommonUtil.replaceOperator(daoUriHash);
            String fileName = daoUriHash + ".json";
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            newProjectUriDto
                    .setUri(urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName + "/" + fileName);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(newProjectUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName,
                    representaion);
            s3FileName = urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName + "/" + fileName;
            log.info("[createBasicDao] s3FileName:{}", s3FileName);

        } catch (Exception e) {
            log.error("[createDao] upload newProjectUriDto error bacisDaoCreateReqVo:{} e:{}", JacksonUtil.obj2json(bacisDaoCreateReqVo),
                    e);
            result.setResultDesc("network error please try again later.");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        if (StringUtils.isBlank(ProtoDaoConstant.CURRENT_ROUND)) {
            log.error("[createBasicDao] current_round is null");
            result.setResultDesc("system exception, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        int periodDays = 0;
        DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        if (StringUtils.isNotBlank(bacisDaoCreateReqVo.getDaoStartDate())) {
            LocalDate dateParam = LocalDate.parse(bacisDaoCreateReqVo.getDaoStartDate(), df);
            Period period = Period.between(LocalDate.now(), dateParam);
            periodDays = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
            periodDays = Math.max(periodDays, 0);
        }

        BasicDaoCreateResVo daoCreateResVo = new BasicDaoCreateResVo();
        daoCreateResVo.setStartDrb(Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) + periodDays);
        if (periodDays > 0) {

            LocalDate dateParam2 = LocalDate.parse(bacisDaoCreateReqVo.getDaoStartDate(), df);
            Date yesterday = DateUtil.addDay(new Date(), -1);
            LocalDate dateParam = LocalDate.parse(sdf.format(yesterday), df);
            String yesterdayTime = DateUtil.getThisDayBeginTime(dateParam);
            String yesterdayBlockNo = ProtoDaoCommonUtil.timestampBlockNo(new BigDecimal(yesterdayTime).divide(new BigDecimal("1000"), 0, BigDecimal.ROUND_UP).toPlainString());
            log.info("[createBasicDao] yesterdayTime:{} yesterdayBlockNo:{} ", yesterdayTime, yesterdayBlockNo);
            if (StringUtils.isNotBlank(yesterdayBlockNo)) {

                Period period = Period.between(dateParam, dateParam2);
                int periods = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
                periods = Math.max(periods, 0);
                String startBlock = CommonUtil.calculateStartBlockNo(yesterdayBlockNo, periods).toString();
                startBlock = new BigDecimal(startBlock).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 0, RoundingMode.HALF_UP).toPlainString();
                log.info("[createBasicDao] yesterdayBlockNo:{} periods:{} startBlock:{}", yesterdayBlockNo, periods, startBlock);
                daoCreateResVo.setStartBlock(startBlock);

            } else {
                //计算相差的天数
//                LocalDate dateParam1 = LocalDate.parse(ProtoDaoConstant.etherscanBlockDate, df);
//
//                Period period = Period.between(dateParam1, dateParam2);
//                int periods = period.getDays() + period.getMonths() * 30 + period.getYears() * 365;
//                periods = Math.max(periods, 0);
//                String startBlock = CommonUtil.calculateStartBlockHeight(periods).toString();
//                log.info("[createBasicDao] dateParam1 periods:{} startBlock:{}", periods, startBlock);
//                daoCreateResVo.setStartBlock(startBlock);
                LocalDateTime midnight = LocalDateTime.of(dateParam2, LocalTime.MIDNIGHT);
                ZonedDateTime targetMidnight = midnight.atZone(ZoneId.systemDefault());
                ZonedDateTime now = ZonedDateTime.now(ZoneId.systemDefault());  // 获取时间?
                Duration duration = Duration.between(now, targetMidnight);
                log.info("选定时间到当前时间到小时数为:" + duration.toHours());

                Result<String> resultBlockNum = subscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
                if (resultBlockNum.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
                    result.setResultCode(ResultDesc.ERROR.getResultCode());
                    result.setResultDesc("network anomaly！ please try again later!");
                    return result;
                }
                BigDecimal blockNumber = new BigDecimal(CommonUtil.hexToTenString(resultBlockNum.getData())); // 当前区块数
                BigDecimal block = blockNumber.add(new BigDecimal(CommonUtil.calculateStartBlockHeight(duration.toHours())));
                daoCreateResVo.setStartBlock(String.valueOf(block));

            }


        }
        if (bacisDaoCreateReqVo.getDuration() != null) {
            daoCreateResVo.setDuration(new BigInteger(String.valueOf(bacisDaoCreateReqVo.getDuration())).multiply(new BigInteger(ProtoDaoConstant.etherscanBlockNumber)).toString());
        }
        daoCreateResVo.setRoyaltyFee(DaoCreateFeeEnum.ZERO.getActualValue()); // 不用加 ProtoDaoConstant.MINT_D4A_FEE_RATIO
        daoCreateResVo.setProjectUri(s3FileName);

        Integer mintWindow = MintWindowEnum.getValueByIndex(Integer.parseInt(bacisDaoCreateReqVo.getDaoMintWindow()));

        daoCreateResVo.setMintableRounds(mintWindow);
        //1.4这个改为直接传价格，不需要转下标了
//        daoCreateResVo.setFloorPriceRank(Integer.valueOf(bacisDaoCreateReqVo.getDaoFloorPrice()));
        daoCreateResVo.setMaxNftRank(Integer.parseInt(bacisDaoCreateReqVo.getTotalNftCasting()));

        //canvasUri
        String canvasId = CommonUtil.removeHexPrefixIfExists(Hash.sha3(daoUriHash + bacisDaoCreateReqVo.getUserAddress()));
        log.info("[createBasicDao] daoUriHash:{} userAddress:{} canvasId:{}", daoUriHash, bacisDaoCreateReqVo.getUserAddress(), canvasId);
        CanvasCreateReqVo canvasCreateReqVo = new CanvasCreateReqVo();
        try {
            canvasCreateReqVo.setUserAddress(bacisDaoCreateReqVo.getUserAddress());
            NewCanvasUriDto newCanvasUriDto = NewCanvasUriDto.transfer(canvasCreateReqVo);
            String fileName = canvasId + ".json";
            fileName = CommonUtil.replaceOperator(fileName);
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);

            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(newCanvasUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.canvasBucketName,
                    representaion);

            canvasCreateReqVo.setCanvasUri(
                    urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.canvasBucketName + "/" + fileName);
            log.info("[canvas-create] canvas uri:{}", urlPrefix + fileName);

        } catch (Exception e) {
            log.error("[canvas-create] s3 canvas uri error e", e);
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("please try again later!");
            return result;
        }
        daoCreateResVo.setDaoName(bacisDaoCreateReqVo.getDaoName());
        daoCreateResVo.setCanvasUri(canvasCreateReqVo.getCanvasUri());
        daoCreateResVo.setCanvasId(CommonUtil.addHexPrefixIfNotExist(canvasId));
        daoCreateResVo.setCreateProjectFee(new BigDecimal(String.valueOf(ProtoDaoConstant.CREATE_PROJECT_FEE)).doubleValue());

        String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
        String nftName = CommonUtil.generateNftName(useraddress, bacisDaoCreateReqVo.getDaoName());
        String imageFileNameAws = "0-nft.png";
        String zeroNftImageUrl = urlPrefix + ProtoDaoConstant.nftZero + "/" + nftName + "/" + imageFileNameAws;
        daoCreateResVo.setZeroNftImageUrl(zeroNftImageUrl);
        log.info("[createBasicDao] return BasicDaoCreateResVo:{}", JacksonUtil.obj2json(daoCreateResVo));

        result.setData(daoCreateResVo);
        return result;
    }


    /**
     * 查询该work是否创建过canvas version_2.0
     * result -> data 为true代表存在，为false代表不存在
     */
    @PostMapping(value = "/canvas/exists")
    public Result<Boolean> canvsExists(HttpServletRequest request, @RequestBody(required = false) DaoCanvasExistsReqVo daoCanvasExistsReqVo) {

        Result<Boolean> result = new Result<>();

        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(useraddress)) {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.getById(daoCanvasExistsReqVo.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        Work work = workService.selectWorkById(daoCanvasExistsReqVo.getWorkId());
        if (work == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("Work is not exist!");
            return result;
        }
        String daoUri = dao.getDaoUri();
        String daoUriHash = daoUri.substring(daoUri.lastIndexOf('/') + 1, daoUri.lastIndexOf('.'));
        log.info("[canvsExists] workId:{} daoUriHash:{} useraddress:{}", daoCanvasExistsReqVo.getWorkId(), daoUriHash, work.getCreatorAddress());
        String canvasId = CommonUtil.removeHexPrefixIfExists(Hash.sha3(daoUriHash + work.getCreatorAddress()));
        log.info("[canvsExists] workId:{} canvasId:{}", daoCanvasExistsReqVo.getWorkId(), canvasId);
        Canvas canvas = canvasService.selectCanvasDetailByCanvasId(canvasId);
        result.setData(canvas != null);
        return result;

    }

    /**
     * 通过交易hash查询daoID version_2.0
     */
    @PostMapping(value = "/transaction/hash")
    public Result<Integer> transactionHashForDao(HttpServletRequest request, @RequestBody(required = false) DaoTransactionHashReqVo daoTransactionHashReqVo) {

        Result<Integer> result = new Result<>();
        log.info("[transactionHashForDao] transactionHash:{}", daoTransactionHashReqVo.getTransactionHash());

        //通过交易hash查询dao信息，用于发完交易跳转dao详情
        Dao dao = daoService.selectDaoByTransactionHash(daoTransactionHashReqVo.getTransactionHash());
        if (dao != null) {
            result.setData(dao.getId());
        }
        return result;

    }

    /**
     * protodao 的DAO列表 version_2.0
     */
    @PostMapping(value = "/list/protodao")
    public ResultList<DaoNameListVo> protoDaoList(HttpServletRequest request) {

        ResultList<DaoNameListVo> result = new ResultList<>();
        result.setDataList(new ArrayList<>());
        //查询所有的dao信息按照名称倒序 z-a
        List<Dao> daoList = daoService.protoDaoList();
        if (daoList.size() > 0) {
            List<DaoNameListVo> daoNameListVo = daoList.stream().map(DaoNameListVo::transfer).collect(Collectors.toList());
            result.setDataList(daoNameListVo);
        }
        return result;

    }

    /**
     * protodao 的Dao Member接口 version_2.0
     */
    @PostMapping(value = "/protodao/member")
    public ResultList<DaoNameListVo> protoDaoMemberList(HttpServletRequest request) {

        ResultList<DaoNameListVo> result = new ResultList<>();
        result.setDataList(new ArrayList<>());
        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(useraddress)) {
            return result;
        }
        Set<Dao> daoSet = new HashSet<>();
        //1.用户创建了dao
        List<Dao> daoList = daoService.myDaoListAll(useraddress);
        if (daoList.size() > 0) {
            daoSet.addAll(daoList);
        }
        //2. 查询是否有铸造nft的dao
        List<Integer> daoIdList = workService.selectDaoMemberWork(useraddress);
        if (daoIdList.size() > 0) {
            List<Dao> daoMemberList = daoService.listByIds(daoIdList);
            if (daoMemberList.size() > 0) {
                daoSet.addAll(daoMemberList);
            }
        }
        //按创建时间倒序
        if (daoSet.size() > 0) {
            List<DaoNameListVo> daoNameListVo = daoSet.stream().map(DaoNameListVo::transfer)
                    .sorted(Comparator.comparing(DaoNameListVo::getSorted).reversed()).collect(Collectors.toList());
            result.setDataList(daoNameListVo);
        }
        return result;

    }


    /**
     * 创建work页面选择Dao后，查询返回Unminted Works及DAO Creator Fee等信息 version_1.1.1-返回使用data数据
     */
    @PostMapping(value = "/createWork/info")
    public Result<CanvasForCreateWorkResVo> createWorkInfo(HttpServletRequest request, @RequestBody(required = false) DaoSortedReqVo daoSortedReqVo) {

        Result<CanvasForCreateWorkResVo> result = new Result<>();
        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

        log.info("[daocreateWorkInfo] daoId:{}", daoSortedReqVo.getDaoId());
        Dao dao = daoService.getById(daoSortedReqVo.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Dao is not exist!");
            return result;
        }
        IPage<Work> workIPage = new Page<>(1, 3);
        daoSortedReqVo.setProjectId(dao.getProjectId());
        Page<Work> workPage = workService.selectUnmintedWorkByProjectId(workIPage, daoSortedReqVo);
        Canvas canvas = null;
        String canvasId = "";
        if (StringUtils.isNotBlank(useraddress)) {
            String daoUriHash = dao.getDaoUri().substring(dao.getDaoUri().lastIndexOf("/") + 1, dao.getDaoUri().lastIndexOf("."));
            canvasId = CommonUtil.removeHexPrefixIfExists(Hash.sha3(daoUriHash + useraddress));
            canvas = canvasService.selectCanvasDetailByCanvasId(canvasId);
        }
        if (canvas == null) {
            canvas = new Canvas();
            if (StringUtils.isBlank(canvasId)) {
                canvas.setCanvasId("");
            } else {
                canvas.setCanvasId(CommonUtil.addHexPrefixIfNotExist(canvasId));
            }
            if (dao.getCanvasFloorPrice() != null && dao.getCanvasFloorPrice().compareTo(dao.getDaoFloorPrice()) < 0) {
                canvas.setCurrentPrice(dao.getCanvasFloorPrice());
            } else {
                canvas.setCurrentPrice(dao.getDaoFloorPrice());
            }
            canvas.setRoyaltyToken(null);
        }


        CanvasForCreateWorkResVo canvasForCreateWorkResVo = CanvasForCreateWorkResVo.transfer(dao, canvas, workPage);

        result.setData(canvasForCreateWorkResVo);
        return result;
    }

    /**
     * protodao 的Dao Member接口 date 0-无权限 1-4有权限，大于等于5 超过限制 version_2.0
     */
    @PostMapping(value = "/protodao/authority")
    public Result<Integer> protoDaoAuthority(HttpServletRequest request, @RequestBody(required = false) DaoReqVo daoReqVo) {


        Result<Integer> result = new Result<>();
        result.setData(1);

        return result;
//        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId())) {
//            log.info("[protoDaoAuthority] daoReqVo is null");
//            return result;
//        }
//        log.info("[protoDaoAuthority] daoId:{}", daoReqVo.getDaoId());
//        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
//        if (StringUtils.isBlank(userAddress)) {
//            return result;
//        }
//        Dao dao = daoService.getById(daoReqVo.getDaoId());
//        if (dao == null) {
//            return result;
//        }
//
//        if (BasicDaoEnum.PROTO_DAO.getBasicType().equals(dao.getBasicDao())) {
//            result.setData(1);
//            return result;
//        }
//
//        int workNum = workService.selectCountMintByAddressAndDaoId(userAddress, Integer.valueOf(daoReqVo.getDaoId()));
//        if (workNum == 0 && userAddress.equals(dao.getOwnerAddress())) {
//            result.setData(1);
//            return result;
//        }
//        if (workNum == 0 && !userAddress.equals(dao.getOwnerAddress())) {
//            workNum = workService.selectCountHoldByAddressAndDaoId(userAddress, Integer.valueOf(daoReqVo.getDaoId()));
//            if (workNum > 0) {
//                result.setData(1);
//                return result;
//            }
//        }
//        result.setData(workNum);
//        return result;

    }


    /**
     * 1.3 包含除了自己之外所有这一系列DAO下的dao
     */
    @PostMapping(value = "/allcation/list")
    public ResultList<DaoNameListVo> daoAllcationList(HttpServletRequest request, @RequestBody(required = false) DaoReqVo daoReqVo) {
        /*
         * 1. 包含除了自己之外所有这一系列DAO下的dao
         * 2. 包含main dao（这里指的是main dao的asset pool而不是redeem）
         * 3. 开启top-up模式的dao不会出现在ETH allocation to each DAO这个列表里  z这个前端做了判断
         * 4. 结束的dao或者未开始的dao出现在列表里
         * */
        ResultList<DaoNameListVo> result = new ResultList<>();
        result.setDataList(new ArrayList<>());

        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId())) {
            log.info("[daoAllcationList] daoReqVo is null");
            return result;
        }

        log.info("[daoAllcationList] daoId:{}", daoReqVo.getDaoId());

        Dao dao = daoService.getById(daoReqVo.getDaoId());
        if (dao == null) {
            log.warn("[daoAllcationList] dao is null");
            return result;
        }

        String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        List<Dao> daoList = daoService.selectByExistDaoId(projectId);
        if (TrueOrFalseEnum.TRUE.getStatus().equals(daoReqVo.getType())) {
            daoList = daoList.stream().filter(v -> !Integer.valueOf(daoReqVo.getDaoId()).equals(v.getId())).collect(Collectors.toList());
        }
        List<DaoNameListVo> daoNameListVos = daoList.stream().map(DaoNameListVo::transfer).collect(Collectors.toList());
        result.setDataList(daoNameListVos);
        return result;

    }


    /**
     * 1.3 判断当前用户是否为dao的creator
     */
    @PostMapping(value = "/maincreator")
    public Result<DaoMainCreatorVo> daoMainCreator(HttpServletRequest request, @RequestBody(required = false) DaoReqVo daoReqVo) {

        Result<DaoMainCreatorVo> result = new Result<>();
        DaoMainCreatorVo daoMainCreatorVo = new DaoMainCreatorVo();
        daoMainCreatorVo.setIsCreator(false);
        daoMainCreatorVo.setRemainingDaoToken(new BigDecimal(ProtoDaoConstant.ERC20_TOTAL));
        result.setData(daoMainCreatorVo);
        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId())) {
            log.info("[daoMainCreator] daoReqVo is null");
            return result;
        }

        log.info("[daoMainCreator] daoId:{}", daoReqVo.getDaoId());

        Dao dao = daoService.getById(daoReqVo.getDaoId());
        if (dao == null) {
            return result;
        }
        daoMainCreatorVo.setErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();

        daoMainCreatorVo.setExistDaoId(CommonUtil.addHexPrefixIfNotExist(StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId()));
        Long totalErc20Token = daoService.selectTotalDaoTokenByErc20Token(dao.getErc20Token());
        daoMainCreatorVo.setRemainingDaoToken(new BigDecimal(ProtoDaoConstant.ERC20_TOTAL).subtract(new BigDecimal(String.valueOf(totalErc20Token))));
        daoMainCreatorVo.setIsThirdpartyToken(Integer.valueOf(1).equals(dao.getIsThirdpartyToken()));
        // 1.7 添加参数
        daoMainCreatorVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoMainCreatorVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoMainCreatorVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoMainCreatorVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoMainCreatorVo.setDaoSymbol(dao.getDaoSymbol());
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            return result;
        }

        Dao mainDao = daoService.daoDetailByProjectId(projectId);
        if (mainDao != null && userAddress.equals(mainDao.getOwnerAddress())) {
            daoMainCreatorVo.setIsCreator(true);
        }
        if (mainDao != null) {
            daoMainCreatorVo.setErc20Address(CommonUtil.addHexPrefixIfNotExist(mainDao.getErc20Token()));
        }

        return result;

    }

    /**
     * 1.3 subDao AssetPool转账入口
     * 1.4 新增了eth相关信息
     */
    @PostMapping(value = "/allocation")
    public Result<DaoAssetAllocationVo> daoAllocation(HttpServletRequest request, @RequestBody(required = false) DaoReqVo daoReqVo) {

        Result<DaoAssetAllocationVo> result = new Result<>();
        DaoAssetAllocationVo daoAssetAllocationVo = new DaoAssetAllocationVo();
        result.setData(daoAssetAllocationVo);
        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId())) {
            log.info("[daoAllocation] daoReqVo is null");
            return result;
        }

        log.info("[daoAllocation] daoId:{}", daoReqVo.getDaoId());

        Dao dao = daoService.getById(daoReqVo.getDaoId());
        if (dao == null) {
            return result;
        }

        daoAssetAllocationVo.setSubDaoAssetPool(CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
        daoAssetAllocationVo.setCurrentDaoToken(commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals()));
        daoAssetAllocationVo.setSubDaoErc20(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));


        //查询assetPool eth的余额
        // daoAssetAllocationVo.setCurrentDaoEth(commonService.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals()));
        daoAssetAllocationVo.setCurrentDaoEth(commonService.getInputToken(dao));

        // 1.7 添加参数
        daoAssetAllocationVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoAssetAllocationVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoAssetAllocationVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoAssetAllocationVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoAssetAllocationVo.setDaoSymbol(dao.getDaoSymbol());
        return result;

    }

    /**
     * 1.3 查询当前topup模式下的eth余额
     */
    @PostMapping(value = "/topup/ethbalance")
    public Result<UserTopupBalanceVo> topupEthBalance(@RequestBody(required = false) DaoReqVo daoReqVo) {
        Result<UserTopupBalanceVo> result = new Result<>();
        UserTopupBalanceVo userTopupBalanceVo = new UserTopupBalanceVo();
        result.setData(userTopupBalanceVo);
        if (daoReqVo == null || StringUtils.isBlank(daoReqVo.getDaoId()) || StringUtils.isBlank(daoReqVo.getUserAddress())) {
            return result;
        }
        String userAddress = daoReqVo.getUserAddress();


        Dao dao = daoService.getById(daoReqVo.getDaoId());
        if (dao == null) {
            return result;
        }

        String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
//        UserTopupHarvest userTopupHarvest = userTopupHarvestService.selectByProjectIdAndUserAddress(projectId, userAddress);
//        if (userTopupHarvest != null) {
//            userTopupBalanceVo.setEthBalance(userTopupHarvest.getEthAmount());
//            userTopupBalanceVo.setTokenBalance(userTopupHarvest.getErc20Amount());
//        }

        WorkTopupHarvest workTopupHarvest = workTopupHarvestService.selectByProjectIdAndUserAddress(projectId, userAddress);
        if (workTopupHarvest != null) {
            userTopupBalanceVo.setEthBalance(workTopupHarvest.getEthAmount());
            userTopupBalanceVo.setTokenBalance(workTopupHarvest.getErc20Amount());
        }

        return result;
    }


    /**
     * 1.3 查询addWork白名单merkle信息
     */
    @PostMapping(value = "/blackandwhite/merkle/work")
    public Result<DaoWhiteMerkelResVo> daoBlackAndWhiteMerkleForWork(@RequestBody WorkIdReqVo workIdReqVo,
                                                                     HttpServletRequest request) {

        Result<DaoWhiteMerkelResVo> result = new Result<>();
        log.info("[daoBlackAndWhiteMerkleForWork]workIdReqVo:{}", JacksonUtil.obj2json(workIdReqVo));
        if (workIdReqVo == null || StringUtils.isAnyBlank(workIdReqVo.getWorkId(), workIdReqVo.getUserAddress())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        DaoWhiteMerkelResVo daoWhiteMerkelResVo = new DaoWhiteMerkelResVo();
        Work work = workService.selectWorkById(workIdReqVo.getWorkId());
        if (work == null || work.getDaoId() == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "work is not exist!");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(work.getDaoId());
        if (dao == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "DAO is not exist!");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        try {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());

            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())
                    && Arrays.asList(daoStrategy.getOriginAddress().split(",")).contains(work.getOwnerAddress())) {

                // 查询proof表
                WhiteList whiteList = whiteListService.getById(daoStrategy.getProofId());
                if (whiteList != null && StringUtils.isNotBlank(whiteList.getProof())) {
                    // List<Map> merkelTreeDtoList = JacksonUtil.json2list(whiteList1.getProof(), Map.class);
                    // Optional<Map> optionalMap = merkelTreeDtoList.stream()
                    // .filter(v -> v.get(daoIdReqVo.getUserAddress()) != null).findFirst();
                    // if (optionalMap.isPresent()) {
                    // String proofList = optionalMap.get().get(daoIdReqVo.getUserAddress()).toString();
                    // proofList = proofList.substring(1, proofList.length() - 1);
                    // if (StringUtils.isNotBlank(proofList)) {
                    // daoWhiteMerkelResVo.setProof(Arrays.asList(proofList.replaceAll(" ", "").split(",")));
                    // }
                    // daoWhiteMerkelResVo.setRootHash(whiteList1.getProofRootHash());
                    // }
                    MerkleTree mt = JacksonUtil.json2pojo(whiteList.getProof(), MerkleTree.class);
                    for (LeafValue leaf : mt.getValues()) {
                        if (leaf.getValue().equals(work.getOwnerAddress())) {
                            daoWhiteMerkelResVo.setProof(MerkleTree.getProof(mt.getTree(), leaf.getTreeIndex()));
                            daoWhiteMerkelResVo.setRootHash(whiteList.getProofRootHash());
                            break;
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("[daoBlackAndWhiteMerkleForWork]workIdReqVo:{} e:", JacksonUtil.obj2json(workIdReqVo), e);
            result.setResultDesc("system error, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
        }

        log.info("[daoBlackAndWhiteMerkleForWork]workId:{} userAddress:{} daoWhiteMerkelResVo:{} ", workIdReqVo.getWorkId(), workIdReqVo.getUserAddress(), JacksonUtil.obj2json(daoWhiteMerkelResVo));

        result.setData(daoWhiteMerkelResVo);

        return result;
    }


    /**
     * 1.11 根据id获取dao的导出信息
     */
    @PostMapping(value = "/export/info")
    public Result<DaoExportInfoVo> daoExportInfo(@RequestBody DaoExportParam daoExportParam, HttpServletRequest request) {

        Result<DaoExportInfoVo> result = new Result<>();
        if (daoExportParam == null || StringUtils.isBlank(daoExportParam.getDaoId()) || daoExportParam.getType() == null) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        return daoService.daoExportInfo(daoExportParam);
    }

    /**
     * 1.12 获取系统中所有的input token type
     */
    @PostMapping(value = "/token/type")
    public Result<TokenType> tokenType(@RequestBody SearchReqVo searchReqVo) {
        return daoService.selectTokenType(searchReqVo);
    }


    // ========================私有方法分隔线===========================================//

    /**
     * 判断用户是否有创建canvas权限
     *
     * @param userAddress
     * @param dao
     * @return
     */
    private boolean checkCanvasCreateWhite(String userAddress, Dao dao) {
        if (dao.getCanvasCreatedBlacklist() == 1) {
            // 查询黑名单是否包含
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())
                    && Arrays.asList(daoStrategy.getOriginAddress().split(",")).contains(userAddress)) {
                return false;
            }
        }

        boolean flag = false;
        if (dao.getCanvasCreatedWhitelist() >= 1) {
            if (dao.getCanvasCreatedWhitelist() == 1 || dao.getCanvasCreatedWhitelist() == 3) {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())
                        && Arrays.asList(daoStrategy.getOriginAddress().split(",")).contains(userAddress)) {
                    return true;
                }
            }

            if (dao.getCanvasCreatedWhitelist() == 2 || dao.getCanvasCreatedWhitelist() == 3) {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    // Dao erc721Dao = daoService.selectDaoByErc721Token(daoStrategy.getOriginAddress());
                    // Work erc721Work = workService.findHoldByAddressAndDaoId(userAddress, erc721Dao.getId());
                    // if (erc721Work != null) {
                    // return true;
                    // } else {
                    // return false;
                    // }
                    List<String> erc721List;
                    if (daoStrategy.getOriginAddress().contains("[")) {
                        erc721List = JacksonUtil.json2StringList(daoStrategy.getOriginAddress());
                    } else {
                        erc721List = Collections.singletonList(daoStrategy.getOriginAddress());
                    }
                    if (erc721List != null) {
                        for (String erc721 : erc721List) {
                            flag = erc721BalanceOf(erc721, userAddress, null);
                            if (flag) {
                                break;
                            }
                        }
                    }

                }
            }
            return flag;
        }

        // 确定每一个nft的拥有者是谁--nft build白名单中，这个nft的owner与当前登录用户比较，如果一致，说明当前登录用户在白名单中，那么就有权限，否则没有
        if (dao.getCanvasCreatedWhitelistNft() >= 1) {
            try {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    List<NftIdentifier> erc721IdList;
                    erc721IdList = JacksonUtil.json2list(daoStrategy.getOriginAddress(), NftIdentifier.class);

                    if (erc721IdList != null) {
                        for (NftIdentifier nftIdentifier : erc721IdList) {
                            flag = erc721OwnerOf(nftIdentifier.getErc721Address(), nftIdentifier.getTokenId(), userAddress);
                            if (flag) {
                                break;
                            }
                        }
                    }
                }
            } catch (Exception e) {
                log.error("[checkCanvasCreateWhite] checkCanvasCreateWhite error daoId:{} e:", dao.getId(), e);
            }
            return flag;
        }

        return true;
    }

    /**
     * 判断用户是否有Mint Work权限
     *
     * @param userAddress
     * @param dao
     * @return
     */
    private boolean checkMintWorkWhite(String userAddress, Dao dao) {

        if (dao.getMinterWorksBlacklist() == 1) {
            // 查询黑名单是否包含
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())
                    && Arrays.asList(daoStrategy.getOriginAddress().split(",")).contains(userAddress)) {
                return false;
            }
        }

        boolean flag = false;
        if (dao.getMinterWorksWhitelist() >= 1 || (dao.getMintCap() != null && dao.getMintCap() == 1) || dao.getMinterWorksWhitelistNft() >= 1 || dao.getErc721MintCapId() >= 1 || dao.getErc721MintCap() >= 1) {
            log.info("进入白名单列表:" + userAddress);
            // 高优address白名单
            if (dao.getMintCap() != null && dao.getMintCap() == 1) {
                log.info("进入address高优白名单:" + userAddress);
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    try {
                        List<DesignatedCap> designatedCapList =
                                JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedCap.class);
                        Optional<DesignatedCap> designatedCapOptional = designatedCapList.stream()
                                .filter(v -> userAddress.equalsIgnoreCase(v.getAccount())).findFirst();
                        if (designatedCapOptional.isPresent()) {
                            return true;
                        }
                    } catch (Exception e) {
                        log.error("[daoUserAuthority] checkMintWorkWhite userAddress:{} originAddress:{} e:",
                                userAddress, daoStrategy.getOriginAddress(), e);
                    }
                }
            }

            // mint work address白名单
            if (dao.getMinterWorksWhitelist() == 1 || dao.getMinterWorksWhitelist() == 3) {
                log.info("mint work address白名单:" + userAddress);
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())
                        && Arrays.asList(daoStrategy.getOriginAddress().split(",")).contains(userAddress)) {
                    return true;
                }
            }

            //  mint work nft高优白名单
            if (dao.getErc721MintCapId() != null && dao.getErc721MintCapId() == 1) {
                log.info("高优nft白名单:" + userAddress);
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType());

                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    if (StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                        try {
                            List<DesignatedNftCap> designatedNftCaps =
                                    JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedNftCap.class);

                            for (DesignatedNftCap designatedNftCap : designatedNftCaps) {
                                // 校验 nft属于当前登陆用户
                                flag = erc721OwnerOf(designatedNftCap.getNftAddress(), designatedNftCap.getTokenId(), userAddress);
                                if (flag) {
                                    return true;
                                }
                            }
                        } catch (Exception e) {
                            log.error("[daoUserAuthority] checkMintWorkWhite Erc721MintCapId userAddress:{} originAddress:{} e:",
                                    userAddress, daoStrategy.getOriginAddress(), e);
                        }
                    }
                }
            }

            //  mint work nft白名单
            if (dao.getMinterWorksWhitelistNft() != null && dao.getMinterWorksWhitelistNft() == 1) {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());

                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    if (StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                        try {
                            List<NftIdentifier> nftIdentifiers =
                                    JacksonUtil.json2list(daoStrategy.getOriginAddress(), NftIdentifier.class);

                            for (NftIdentifier nftIdentifier : nftIdentifiers) {
                                // 校验 nft属于当前登陆用户
                                flag = erc721OwnerOf(nftIdentifier.getErc721Address(), nftIdentifier.getTokenId(), userAddress);
                                if (flag) {
                                    return true;
                                }
                            }
                        } catch (Exception e) {
                            log.error("[daoUserAuthority] checkMintWorkWhite Erc721MintCapId userAddress:{} originAddress:{} e:",
                                    userAddress, daoStrategy.getOriginAddress(), e);
                        }
                    }
                }
            }

            // 高优 erc721白名单
            if (dao.getErc721MintCap() != null && dao.getErc721MintCap() == 1) {
                log.info("高优 erc721白名单白名单:" + userAddress);
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    if (StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                        try {
                            List<DesignatedCap> designatedCapList =
                                    JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedCap.class);

                            for (DesignatedCap designatedCap : designatedCapList) {
                                flag = erc721BalanceOfGreaterThanZero(designatedCap.getAccount(), userAddress);
                                if (!flag) {
                                    break;
                                }
                                // 不用做次数限制
//                                flag = daoMintCount(designatedCap.getAccount(), userAddress, designatedCap.getCap());
//                                if (flag) {
//                                    break;
//                                }
                            }
                        } catch (Exception e) {
                            log.error("[daoUserAuthority] checkMintWorkWhite userAddress:{} originAddress:{} e:",
                                    userAddress, daoStrategy.getOriginAddress(), e);
                        }
                    }
                }
                return flag;
            }

            // mint work erc721白名单
            if (dao.getMinterWorksWhitelist() == 2 || dao.getMinterWorksWhitelist() == 3) {
                log.info(" mint work erc721白名单:" + userAddress);
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    // String erc721 = daoStrategy.getOriginAddress();
                    // Dao erc721Dao = daoService.selectDaoByErc721Token(erc721);
                    // List<Work> workList = workService.selectWorksByDaoIdAndStatus(erc721Dao.getId() + "",
                    // WorkStatusEnum.CASTED.getStatus());
                    // List<String> ownerList =
                    // workList.stream().map(Work::getOwnerAddress).collect(Collectors.toList());

                    // Dao erc721Dao = daoService.selectDaoByErc721Token(daoStrategy.getOriginAddress());
                    // Work erc721Work = workService.findHoldByAddressAndDaoId(userAddress, erc721Dao.getId());
                    // if (erc721Work != null) {
                    // return true;
                    // } else {
                    // return false;
                    // }
                    // flag = erc721BalanceOf(daoStrategy.getOriginAddress(), userAddress);
                    List<String> erc721List;
                    if (daoStrategy.getOriginAddress().contains("[")) {
                        erc721List = JacksonUtil.json2StringList(daoStrategy.getOriginAddress());
                    } else {
                        erc721List = Collections.singletonList(daoStrategy.getOriginAddress());
                    }
                    if (erc721List != null) {
                        for (String erc721 : erc721List) {
                            flag = erc721BalanceOf(erc721, userAddress, null);
                            if (flag) {
                                break;
                            }
                        }
                    }
                }
            }
            return flag;
        }

        // 应该是用户铸造的而不是用户持有的
        if (dao.getGlobalMintCap() != null && dao.getGlobalMintCap() > 0) {
            int count = workService.selectCountHoldByAddressAndDaoId(userAddress, dao.getId());
            if (count >= dao.getGlobalMintCap()) {
                return false;
            }
        }

        return true;
    }

    /**
     * erc721BalanceOf
     *
     * @param contract
     * @param address
     * @param mintCount
     * @return
     */
    private boolean erc721BalanceOf(String contract, String address, Integer mintCount) {
        try {
            // 通过ERC721合约的balanceOf查询用户地址是否拥有nft
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(contract);
            String data = ProtoDaoConstant.balanceOf
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(address));
            log.info("[erc721BalanceOf] network:{} contract:{} data:{}", ProtoDaoConstant.netWork, contract, data);
            infuraCallRequestDto.setData(data);
            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[erc721BalanceOf] error result:{} ", result.getResultDesc());
                return true;
            }
            log.info("[erc721BalanceOf]infura return data:{}", result.getData());
            int count = Integer.valueOf(CommonUtil.hexToTenString(result.getData()));
            if (mintCount == null) {
                if (count == 0) {
                    return false;
                } else {
                    return true;
                }
            } else {
                return count > 0 && count < mintCount;
            }

        } catch (Exception e) {
            log.error("[erc721BalanceOf]error daoIdReqVo:{} e:", "contract:" + contract + "\taddress:" + address, e);
            return false;
        }
    }

    /**
     * erc721BalanceOf
     *
     * @param contract     721地址
     * @param tokenId
     * @param checkAddress
     * @return
     */
    private boolean erc721OwnerOf(String contract, Integer tokenId, String checkAddress) {
        try {
            // 通过ERC721合约的balanceOf查询用户地址是否拥有nft
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(contract);
            String data = ProtoDaoConstant.nftOwnerOf
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(tokenId));
            log.info("[erc721OwnerOf] network:{} contract:{} data:{}", ProtoDaoConstant.netWork, contract, data);
            infuraCallRequestDto.setData(data);
            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[erc721OwnerOf] error result:{} ", result.getResultDesc());
                return true;
            }
            log.info("[erc721OwnerOf]infura return data:{}", result.getData());
            String ownerAddress = CommonUtil.formatBytes32Address(result.getData());

            return ownerAddress.equals(CommonUtil.removeHexPrefixIfExists(checkAddress));

        } catch (Exception e) {
            log.error("[erc721BalanceOf]error daoIdReqVo:{} e:", "contract:" + contract + "\tcheckAddress:" + checkAddress, e);
            return false;
        }
    }


    /**
     * erc721BalanceOfGreaterThanZero
     *
     * @param contract
     * @param address
     * @return
     */
    private boolean erc721BalanceOfGreaterThanZero(String contract, String address) {
        try {
            // 通过ERC721合约的balanceOf查询用户地址是否拥有nft
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(contract);
            String data = ProtoDaoConstant.balanceOf
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(address));
            log.info("[erc721BalanceOfGreaterThanZero] network:{} contract:{} data:{}", ProtoDaoConstant.netWork, contract, data);
            infuraCallRequestDto.setData(data);
            Result<String> result = subscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[erc721BalanceOfGreaterThanZero] error result:{} ", result.getResultDesc());
                return true;
            }
            log.info("[erc721BalanceOfGreaterThanZero]infura return data:{}", result.getData());
            int count = Integer.valueOf(CommonUtil.hexToTenString(result.getData()));

            return count > 0;


        } catch (Exception e) {
            log.error("[erc721BalanceOfGreaterThanZero]error daoIdReqVo:{} e:", "contract:" + contract + "\taddress:" + address, e);
            return false;
        }
    }


    /**
     * 下次替换为从合约查询
     * 用这个方法可以查询mint了几次：function getUserMintInfo(bytes32 daoId, address account) public view returns (uint32 minted, uint32 userMintCap)， 返回值中的第一个是已铸造了多少个
     *
     * @param contract
     * @param address
     * @param mintCount
     * @return
     */
    private boolean daoMintCount(String contract, String address, Integer mintCount) {
        try {
            Dao dao = daoService.selectDaoByErc721Token(contract);
            if (dao == null) {
                return false;
            }
            int count = workService.selectCountMintByAddressAndDaoId(address, dao.getId());
            log.info("[daoMintCount]daoId:{} count :{}", dao.getId(), count);
            if (mintCount == null) {
                if (count == 0) {
                    return false;
                } else {
                    return true;
                }
            } else {
                return count < mintCount;
            }

        } catch (Exception e) {
            log.error("[daoMintCount]error daoIdReqVo:{} e:", "contract:" + contract + "\taddress:" + address, e);
            return false;
        }
    }

    /**
     * 构建创建canvas权限
     *
     * @param dao
     * @param daoWhiteListResVo
     * @return
     */
    private DaoWhiteListResVo.Strategy buildCanvasCreateWhite(Dao dao, DaoWhiteListResVo daoWhiteListResVo) {
        DaoWhiteListResVo.Strategy createCanvas = daoWhiteListResVo.new Strategy();
        if (dao.getCanvasCreatedBlacklist() == 1) {
            // 查询黑名单是否包含
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                createCanvas.setBlackList(Arrays.asList(daoStrategy.getOriginAddress().split(",")));
            }
        }

        if (dao.getCanvasCreatedWhitelist() == 1 || dao.getCanvasCreatedWhitelist() == 3) {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                DaoWhiteListResVo.WhiteList whiteList = daoWhiteListResVo.new WhiteList();
                whiteList.setWhiteListAddress(Arrays.asList(daoStrategy.getOriginAddress().split(",")));
                // 查询proof表
                WhiteList whiteList1 = whiteListService.getById(daoStrategy.getProofId());
                whiteList.setRootHash(whiteList1.getProofRootHash());
                createCanvas.setWhiteList(whiteList);
            }
        }

        if (dao.getCanvasCreatedWhitelist() == 2 || dao.getCanvasCreatedWhitelist() == 3) {

            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                List<String> erc721List;
                if (daoStrategy.getOriginAddress().contains("[")) {
                    erc721List = JacksonUtil.json2StringList(daoStrategy.getOriginAddress());
                } else {
                    erc721List = Collections.singletonList(daoStrategy.getOriginAddress());
                }
                if (erc721List != null) {
                    createCanvas.setWhiteListedERC721(erc721List);
                }
            }
        }

        //  获取create canvas的白名单信息
        log.info("CanvasCreatedWhitelistNft dao info:" + JacksonUtil.obj2json(dao));
        if (dao.getCanvasCreatedWhitelistNft() == 1) {
            try {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    List<NftIdentifier> erc721IdList;
                    erc721IdList = JacksonUtil.json2list(daoStrategy.getOriginAddress(), NftIdentifier.class);
                    createCanvas.setWhiteListedERC721Id(erc721IdList);
                }
            } catch (Exception e) {
                log.error("[buildCanvasCreateWhite] setWhiteListedERC721Id error daoId:{} e:", dao.getId(), e);
                createCanvas.setWhiteListedERC721Id(new ArrayList<>());
            }

        }

        return createCanvas;
    }

    /**
     * 构建dao的Mint Work权限
     *
     * @param dao
     * @return
     */
    private DaoWhiteListResVo.Strategy buildMintWorkWhite(Dao dao, DaoWhiteListResVo daoWhiteListResVo) {
        DaoWhiteListResVo.Strategy minting = daoWhiteListResVo.new Strategy();

        if (dao.getMinterWorksBlacklist() == 1) {
            // 查询黑名单是否包含
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                minting.setBlackList(Arrays.asList(daoStrategy.getOriginAddress().split(",")));
            }
        }

        if (dao.getMinterWorksWhitelist() == 1 || dao.getMinterWorksWhitelist() == 3) {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                DaoWhiteListResVo.WhiteList whiteList = daoWhiteListResVo.new WhiteList();
                whiteList.setWhiteListAddress(Arrays.asList(daoStrategy.getOriginAddress().split(",")));
                // 查询proof表
                WhiteList whiteList1 = whiteListService.getById(daoStrategy.getProofId());
                whiteList.setRootHash(whiteList1.getProofRootHash());
                minting.setWhiteList(whiteList);
            }
        }

        if (dao.getMinterWorksWhitelist() == 2 || dao.getMinterWorksWhitelist() == 3) {
            DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                    DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
            if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                List<String> erc721List;
                if (daoStrategy.getOriginAddress().contains("[")) {
                    erc721List = JacksonUtil.json2StringList(daoStrategy.getOriginAddress());
                } else {
                    erc721List = Collections.singletonList(daoStrategy.getOriginAddress());
                }
                if (erc721List != null) {
                    minting.setWhiteListedERC721(erc721List);
                }
            }
        }

        // 处理 mint work的erc721下nft白名单信息
        if (dao.getMinterWorksWhitelistNft() == 1) {
            try {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721_NFT.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    List<NftIdentifier> erc721IdList;
                    erc721IdList = JacksonUtil.json2list(daoStrategy.getOriginAddress(), NftIdentifier.class);
                    minting.setWhiteListedERC721Id(erc721IdList);
                }
            } catch (Exception e) {
                log.error("[buildMintWorkWhite] setWhiteListedERC721Id error daoId:{} e:", dao.getId(), e);
                minting.setWhiteListedERC721Id(new ArrayList<>());
            }
        }


        if (dao.getMintCap() != null && dao.getMintCap() == 1) {
            try {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    minting.setDesignatedMintCaps(
                            JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedCap.class));
                }
            } catch (Exception e) {
                log.error("[buildMintWorkWhite] mintCap error daoId:{} e:", dao.getId(), e);
                minting.setDesignatedMintCaps(new ArrayList<>());
            }
        }

        if (dao.getErc721MintCap() != null && dao.getErc721MintCap() == 1) {
            try {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    minting.setErc721MintCaps(
                            JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedCap.class));
                }
            } catch (Exception e) {
                log.error("[buildMintWorkWhite] setErc721MintCaps error daoId:{} e:", dao.getId(), e);
                minting.setDesignatedMintCaps(new ArrayList<>());
            }
        }

        // 处理erc721下nft的高优白名单
        if (dao.getErc721MintCapId() != null && dao.getErc721MintCapId() == 1) {
            try {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType());
                if (daoStrategy != null && StringUtils.isNotBlank(daoStrategy.getOriginAddress())) {
                    minting.setErc721MintIdCaps(
                            JacksonUtil.json2list(daoStrategy.getOriginAddress(), DesignatedNftCap.class));
                }
            } catch (Exception e) {
                log.error("[buildMintWorkWhite] setErc721MintIdCaps error daoId:{} e:", dao.getId(), e);
                minting.setErc721MintIdCaps(new ArrayList<>());
            }
        }

        if (dao.getGlobalMintCap() != null) {
            minting.setMaxMintingAmount(dao.getGlobalMintCap());
        }

        return minting;
    }

    /**
     * checkMainDaoOwner
     *
     * @param dao
     * @param userAddress
     * @return
     */
    private Boolean checkMainDaoOwner(Dao dao, String userAddress) {
        if (StringUtils.isNotBlank(dao.getExistDaoId())) {
            Dao mainDao = daoService.daoDetailByProjectId(dao.getExistDaoId());
            if (mainDao != null && mainDao.getOwnerAddress().equalsIgnoreCase(userAddress)) {
                return true;
            }
        } else {
            return dao.getOwnerAddress().equalsIgnoreCase(userAddress);
        }
        return false;
    }

    /**
     * main方法
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {


//        BigDecimal bigDecimal = new BigDecimal("6");
//        String canvasId = Hash.sha3("sidhfsdfo");
//        System.out.println(canvasId);
//        String daoLogo = "abcd.txt";
//        String suf = daoLogo.substring(daoLogo.lastIndexOf("."));
//        System.out.println(daoLogo.substring(daoLogo.lastIndexOf(".")));
//        System.out.println(suf.substring(1));
//        Dao dao = new Dao();
//        dao.setDaoUri("https://test-protodao.s3.ap-southeast-1.amazonaws.com/meta/dao/Nf2XhkrYNBP7ZGru4DiwQ79875.json");
//        String daoUriHash = dao.getDaoUri().substring(dao.getDaoUri().lastIndexOf("/") + 1, dao.getDaoUri().lastIndexOf("."));
//        System.out.println(daoUriHash);
//        dao.setOwnerAddress("0x56dba60a326c8a1e1ed148486a2695884aa34e3b");
//        String canvasId = CommonUtil.removeHexPrefixIfExists(Hash.sha3(daoUriHash + dao.getOwnerAddress()));
//        System.out.println(canvasId);

        String proof = "{\"tree\":[\"0x3f672c4ab3c5fbe23de6cca79930a5ca238b06d6cffc2367e48f5a1919638aa2\",\"0x1625d8fdf989774ecd7405db4874f9c8bb88e93aa6053b5d498f1cc12344f0d8\",\"0xf68e52d4f3b3263aaeae567d9308780b11440942da841ef405350e1d79d170f1\",\"0xffa7b9cc247268eae3b8a65cff4cbcc8ef6e671591022019cbbe58cca485d744\",\"0x7e85c49f26ab11fd3f0dd0cd72a06c40c49118585a5ae22aa70d879125c657ca\",\"0x755bd276e5ecf25fd21264f76e232ac456e1636927b51fa1a47f471d421f882c\",\"0x1075d30a5ffef80d593faea4249a32da0d4e196ca0929e0b0c264b4500facd08\"],\"values\":[{\"value\":\"0x130ab1cd93fcbfca39ebc207846344231f00667a\",\"treeIndex\":6},{\"value\":\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"treeIndex\":3},{\"value\":\"0xc8fc1a79f17f11c1cdcb261b759d9886b5fb868d\",\"treeIndex\":5},{\"value\":\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\",\"treeIndex\":4}]}";
        MerkleTree mt = JacksonUtil.json2pojo(proof, MerkleTree.class);
        for (LeafValue leaf : mt.getValues()) {
            if (leaf.getValue().equals("0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8")) {
                System.out.println(MerkleTree.getProof(mt.getTree(), leaf.getTreeIndex()));
            }
        }

//        String originAddress = "[\"0x130ab1cd93fcbfca39ebc207846344231f00667a\",\"0xb90b90225e628188a16c1ab2ffbd8372e49b39df\",\"0xc8fc1a79f17f11c1cdcb261b759d9886b5fb868d\",\"0xf8baf7268f3daefe4135f7711473ae8b6c3b47d8\"]";
//        List<String> list = CommonUtil.checkAddress(originAddress);
//        MerkleTree mt = new MerkleTree(list);
//        mt.init();
//        String rootHash = mt.getRootHash();
//
//        // mintingWhiteList.setOriginAddress(daoWhiteListReqVo.getMintingOriginAddress());
//        System.out.println((JacksonUtil.obj2json(list)));
//        // mintingWhiteList.setProof(daoWhiteListReqVo.getMintingMerkleProof());
//        System.out.println(JacksonUtil.obj2json(mt));
//        System.out.println(rootHash);

        int count = 1;
        System.out.println(count > 0 && count < 1);
    }

}
