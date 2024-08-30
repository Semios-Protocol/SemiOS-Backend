package semios.api.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.SearchNameParamDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Favorites;
import semios.api.model.entity.Work;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.req.WorkId;
import semios.api.model.vo.req.WorkInfo.WorkCurrentWindow;
import semios.api.model.vo.req.WorkInfo.WorkInfo;
import semios.api.model.vo.res.BaseWorkVo.BaseWorkListVo;
import semios.api.service.IDaoService;
import semios.api.service.IFavoritesService;
import semios.api.service.IWorkService;
import semios.api.utils.BeanUtil;
import semios.api.utils.CommonUtil;
import semios.api.utils.CookieUtil;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 1.9.1 优化work展示接口
 *
 * @author zhyyao
 * @order 90
 */
@Slf4j
@RestController
@RequestMapping("/work/show")
public class WorkCollectionController {

    @Autowired
    private IWorkService workService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IFavoritesService favoritesService;


    /**
     * 1.9.1 explore下nft列表与dao详情内的nft列表
     * 原来的work/explore/nfts和/dao/nfts
     */
    @PostMapping(value = "/nft/list")
    public Result<WorkInfo> exploreNft(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                       HttpServletRequest request) {
        Result<WorkInfo> result = new Result<>();
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Work> workPage;
        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            workPage = workService.selectNfts(iPage, daoSortedReqVo);
        } else {
            // dao下nft
            Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
            if (dao == null || StringUtils.isBlank(dao.getProjectId())) {
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("DAO is not exist!");
                return result;
            }
            daoSortedReqVo.setProjectId(dao.getProjectId());
            workPage = workService.selectNftByProjectId(iPage, daoSortedReqVo);
        }

        List<Work> works = workPage.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;
    }


    /**
     * 1.9.1 explore下Mintable Works列表与dao详情内的Mintable Works
     * 原来的work/explore/unmintedWorks和dao/unmintedWorks
     */
    @PostMapping(value = "/works/list")
    public Result<WorkInfo> exploreWorks(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                         HttpServletRequest request) {
        Result<WorkInfo> result = new Result<>();
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Work> workPage;

        if (StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            // explore下未铸造work列表
            workPage = workService.unmintedWorks(iPage, daoSortedReqVo);
        } else {
            // dao下未铸造work列表
            Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
            if (dao == null || StringUtils.isBlank(dao.getProjectId())) {
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("DAO is not exist!");
                return result;
            }
            daoSortedReqVo.setProjectId(dao.getProjectId());
            workPage = workService.selectUnmintedWorkByProjectId(iPage, daoSortedReqVo);
        }

        List<Work> works = workPage.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;
    }


    /**
     * 1.9.1 dao详情内的DRB NFTs,当前周期内的nft
     * 原来的dao/drbNfts
     */
    @PostMapping(value = "/drb/nft/list")
    public Result<WorkInfo> drbNftList(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                       HttpServletRequest request) {
        Result<WorkInfo> result = new Result<>();
        Page<Work> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());

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
        daoSortedReqVo.setProjectId(dao.getProjectId());
        Page<Work> workPage = workService.selectDrbNftByProjectId(iPage, daoSortedReqVo);
        List<Work> works = workPage.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;
    }


    /**
     * 我上传的
     * 原来的/work/creator
     */
    @PostMapping(value = "/creator")
    public Result<WorkInfo> workCreator(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<WorkInfo> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findCreatorByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();

        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * nft-hold藏品
     * 原来的/work/hold
     */
    @PostMapping(value = "/hold")
    public Result<WorkInfo> workHole(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<WorkInfo> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findHoldByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();

        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * minted藏品 铸造的藏品 1.6
     * 原来的/work/minted
     */
    @PostMapping(value = "/minted")
    public Result<WorkInfo> workMinted(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<WorkInfo> result = new Result<>();
        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Work> workPage = workService.findMintedByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();

        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);

        return result;
    }


    /**
     * 1.9.1 搜索返回work结果
     */
    @PostMapping(value = "/search")
    public Result<WorkInfo> searchWorkResult(@RequestBody(required = false) SearchReqVo searchReqVo,
                                             HttpServletRequest request) {

        Result<WorkInfo> result = new Result<>();
        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setCount(0L);

        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            result.setPage(page);
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchWorkResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            result.setPage(page);
            return result;
        }
        Page<Work> iPage = new Page<>(searchReqVo.getPageNo(), searchReqVo.getPageSize());
        List<Work> workList = new ArrayList<>();

        if (CommonUtil.patternWork(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = CommonUtil.patternName(searchReqVo.getSearchWord());
            Work work = workService.selectByNumber(searchNameParamDto.getDaoNumber(),
                    searchNameParamDto.getCnavasNumber(), searchNameParamDto.getWorkNumber());
            if (work != null) {
                page.setCount(1);
                workList.add(work);
            }
        } else {
            if (CommonUtil.patternDao(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                result.setPage(page);
                return result;
            } else {
                Page<Work> workPage = workService.searchWork(iPage, searchReqVo.getSearchWord());
                workList = workPage.getRecords();
                page.setCount(workPage.getTotal());
            }
        }

        if (workList.isEmpty()) {
            result.setDataList(new ArrayList<>());
            result.setPage(page);
            return result;
        }
//        if (searchReqVo.getNumber() != null && workList.size() > searchReqVo.getNumber()) {
//            workList = workList.subList(0, searchReqVo.getNumber());
//        }

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            List<Favorites> favoritesList =
                    favoritesService.findListByUserAddress(FavoriteTypeEnum.WORK_FAVORITE.getType(), userAddress);
            favoritesIds =
                    favoritesList.stream().map(Favorites::getFavoriteId).map(Integer::new).collect(Collectors.toList());
        }
        List<Integer> finalFavoritesIds = favoritesIds;
        List<WorkInfo> workListVos =
                workList.stream().map(v -> WorkInfo.transfer(v, finalFavoritesIds)).collect(Collectors.toList());

        result.setDataList(workListVos);

        page.setPageNo(searchReqVo.getPageNo());
        page.setPageSize(searchReqVo.getPageSize());
        result.setPage(page);
        return result;
    }


    /**
     * 1.9.1 work 列表下的Current Block Window Information
     */
    @PostMapping(value = "/window")
    public Result<WorkCurrentWindow> workWindowInfo(@RequestBody(required = false) WorkId workId,
                                                    HttpServletRequest request) {
        Result<WorkCurrentWindow> result = new Result<>();
        Work work = workService.selectWorkById(workId.getWorkId());
        if (work == null || work.getDaoId() == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("work not exist");
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(work.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Work and DAO is not exist!");
            return result;
        }

        BaseWorkListVo baseWorkListVo = BaseWorkListVo.transfer(work, dao);
        WorkCurrentWindow workCurrentWindow = new WorkCurrentWindow();
        BeanUtil.copyProperties(baseWorkListVo, workCurrentWindow);
        result.setData(workCurrentWindow);
        return result;
    }


}
