package semios.api.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
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
import semios.api.model.dto.common.ResultList;
import semios.api.model.dto.response.SearchNameParamDto;
import semios.api.model.entity.Dao;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.vo.req.DaoIdParam;
import semios.api.model.vo.req.DaoInfo.DaoInfoVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.res.BasicInformationVo;
import semios.api.model.vo.res.MintWindowInfoVo;
import semios.api.model.vo.res.ModeStatusVo;
import semios.api.service.IDaoService;
import semios.api.service.IFavoritesService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.CookieUtil;
import semios.api.utils.JacksonUtil;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 1.9.1 优化dao展示接口
 *
 * @author zhyyao
 * @order 80
 */
@Slf4j
@RestController
@RequestMapping("/dao/show")
public class DaoCollectionController {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IFavoritesService favoritesService;

    @Autowired
    private CommonService commonService;


    /**
     * 1.9.1 collections下的dao列表--仅返回外层dao信息
     * 不用传daoID
     * 原来的 /dao/collections
     */
    @PostMapping(value = "/info")
    public ResultList<DaoInfoVo> daoCollections(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                HttpServletRequest request) {
        ResultList<DaoInfoVo> result = new ResultList<>();

        Page<Dao> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Dao> daoServiceCollectionsDaoList = daoService.getCollectionsDaoList(iPage, daoSortedReqVo);
        List<Dao> daoList = daoServiceCollectionsDaoList.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<DaoInfoVo> daoInfoVoList = daoList.stream()
                .map(v -> DaoInfoVo.transfer(v, finalFavoritesIds))
                .collect(Collectors.toList());
        if (daoInfoVoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            result.setDataList(daoInfoVoList);
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(daoServiceCollectionsDaoList.getTotal());
        result.setPage(page);

        return result;

    }


    /**
     * 1.9.1 collections下 Current Block Window Information
     */
    @PostMapping(value = "/window")
    public Result<MintWindowInfoVo> mintWindowInfo(@RequestBody(required = false) DaoIdParam daoIdParam,
                                                   HttpServletRequest request) {
        Result<MintWindowInfoVo> result = new Result<>();

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdParam.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        MintWindowInfoVo mintWindowInfo = commonService.getMintWindowInfoVo(dao);
        log.info("mintWindowInfo:{}", JacksonUtil.obj2json(mintWindowInfo));

        mintWindowInfo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));
        result.setData(mintWindowInfo);

        return result;
    }


    /**
     * 1.9.1 collections下 Current Block Window Information
     */
    @PostMapping(value = "/basic")
    public Result<BasicInformationVo> daoBasicInfo(@RequestBody(required = false) DaoIdParam daoIdParam,
                                                   HttpServletRequest request) {
        Result<BasicInformationVo> result = new Result<>();

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdParam.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        BasicInformationVo basicInformationVo = commonService.getBasicInformationVo(dao);
        log.info("basicInformationVo:{}", JacksonUtil.obj2json(basicInformationVo));
        result.setData(basicInformationVo);

        return result;
    }

    /**
     * 1.9.1 collections下Mode Status
     */
    @PostMapping(value = "/modeStatus")
    public Result<ModeStatusVo> modeStatus(@RequestBody(required = false) DaoIdParam daoIdParam,
                                           HttpServletRequest request) {
        Result<ModeStatusVo> result = new Result<>();

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdParam.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        ModeStatusVo modeStatusVo = ModeStatusVo.transfer(dao);
        log.info("modeStatusVo:{}", JacksonUtil.obj2json(modeStatusVo));
        result.setData(modeStatusVo);

        return result;
    }


    /**
     * 1.9.1 聚合dao页面下所有的dao信息
     * 需要传daoID和分页信息
     * 原来的 /dao/analytics/togetherDao/list
     */
    @PostMapping(value = "/togetherDao/list")
    public ResultList<DaoInfoVo> daoListByTogetherId(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo,
                                                     HttpServletRequest request) {
        ResultList<DaoInfoVo> result = new ResultList<>();
        if (daoSortedReqVo == null || StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null || dao.getIsTogetherDao() == null || TrueOrFalseEnum.FALSE.getStatus().equals(dao.getIsTogetherDao())) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        Page<Dao> iPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        Page<Dao> daoListByTogetherDaoId = daoService.getDaoListByTogetherDaoId(iPage, daoSortedReqVo.getDaoId());

        List<Dao> daoList = daoListByTogetherDaoId.getRecords();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;


        List<DaoInfoVo> daoInfoVoList = daoList.stream()
                .map(v -> DaoInfoVo.transfer(v, finalFavoritesIds))
                .collect(Collectors.toList());
        if (daoInfoVoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            result.setDataList(daoInfoVoList);
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(daoListByTogetherDaoId.getTotal());
        result.setPage(page);

        return result;
    }


    /**
     * 1.9.1dao详情下的Related Nodes
     * 原来的protodao/related
     */
    @PostMapping(value = "/related")
    public ResultList<DaoInfoVo> protodaoRelated(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo, HttpServletRequest request) {
        ResultList<DaoInfoVo> result = new ResultList<>();

        if (daoSortedReqVo == null || StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }

        String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        IPage<Dao> daoIPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setErc20Token(dao.getErc20Token());
        daoSortedReqVo.setProjectId(projectId);

        Page<Dao> daoPage = daoService.selectDaoListByErc20Token(daoIPage, daoSortedReqVo);
        List<Dao> daoList = daoPage.getRecords();

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<DaoInfoVo> daoInfoVoList = daoList.stream()
                .map(v -> DaoInfoVo.transfer(v, finalFavoritesIds))
                .collect(Collectors.toList());
        if (daoInfoVoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            result.setDataList(daoInfoVoList);
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(daoPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 当前用户dao列表- 返回数据在dataList中
     * 原来的dao/mydao
     */
    @PostMapping(value = "/mydao")
    public Result<DaoInfoVo> myDao(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo, HttpServletRequest request) {

        Result<DaoInfoVo> result = new Result<>();
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress)) {
            result.setResultCode(ResultDesc.SESSION_ERROR.getResultCode());
            result.setResultDesc("please login!");
            return result;
        }

        Page<Dao> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Dao> daoPage = daoService.myDaoList(iPage, userProfilePageReqVo.getUserAddress());
        List<Dao> daoList = daoPage.getRecords();

        List<Integer> favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userProfilePageReqVo.getUserAddress());
        List<DaoInfoVo> daoInfoVoList = daoList.stream()
                .map(v -> DaoInfoVo.transfer(v, favoritesIds))
                .collect(Collectors.toList());
        if (daoInfoVoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            result.setDataList(daoInfoVoList);
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(daoPage.getTotal());
        result.setPage(page);

        return result;
    }


    /**
     * 1.9.1 search dao接口
     * 需要传daoID和分页信息
     * 原来的 /search/daos
     */
    @PostMapping(value = "/search")
    public Result<DaoInfoVo> searchDao(@RequestBody(required = false) SearchReqVo searchReqVo,
                                       HttpServletRequest request) {

        Result<DaoInfoVo> result = new Result<>();
        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setCount(0L);

        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            result.setPage(page);
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchDaoResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            result.setPage(page);
            return result;
        }

        Page<Dao> iPage = new Page<>(searchReqVo.getPageNo(), searchReqVo.getPageSize());

        List<Dao> daoList = new ArrayList<>();
        if (CommonUtil.patternDao(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = CommonUtil.patternName(searchReqVo.getSearchWord());
            Dao dao = daoService.selectDaoByDaoNumber(searchNameParamDto.getDaoNumber().intValue());
            if (dao != null) {
                // 只会查询一条数据
                page.setCount(1);
                daoList.add(dao);
            }
        } else {
            if (CommonUtil.patternWork(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                result.setPage(page);
                return result;
            }
            Page<Dao> daoPage = daoService.searchDao(iPage, searchReqVo.getSearchWord());
            daoList = daoPage.getRecords();
            page.setCount(daoPage.getTotal());
        }

        if (daoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
            result.setPage(page);
            return result;
        }

//        if (searchReqVo.getNumber() != null && daoList.size() > searchReqVo.getNumber()) {
//            daoList = daoList.subList(0, searchReqVo.getNumber());
//        }

        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        List<Integer> favoritesIds = new ArrayList<>();
        if (StringUtils.isNotBlank(userAddress)) {
            favoritesIds = favoritesService.findIdListByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), userAddress);
        }
        List<Integer> finalFavoritesIds = favoritesIds;

        List<DaoInfoVo> daoInfoVoList = daoList.stream()
                .map(v -> DaoInfoVo.transfer(v, finalFavoritesIds))
                .collect(Collectors.toList());
        if (daoInfoVoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            result.setDataList(daoInfoVoList);
        }

        page.setPageNo(searchReqVo.getPageNo());
        page.setPageSize(searchReqVo.getPageSize());
        result.setPage(page);
        return result;
    }


}
