package semios.api.controller;

import com.amazonaws.util.Md5Utils;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;
import semios.api.interceptor.S3Service;
import semios.api.model.annotation.RepeatSubmit;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.response.NewCanvasUriDto;
import semios.api.model.entity.*;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.model.enums.WorkStatusEnum;
import semios.api.model.vo.PageVo;
import semios.api.model.vo.req.*;
import semios.api.model.vo.res.*;
import semios.api.service.*;
import semios.api.utils.*;

import javax.servlet.http.HttpServletRequest;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * canvas画布相关接口
 *
 * @author xiangbin
 * @order 2
 */
@Slf4j
@RestController
@RequestMapping("/canvas")
public class CanvasController {

    private static final RestTemplate restTemplate = new RestTemplate();
    @Autowired
    private IDaoService daoService;
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private IWorkService workService;
    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;
    @Autowired
    private S3Service s3Service;
    @Autowired
    private IFavoritesService favoritesService;

    /**
     * canvas详情 1.9
     */
    @PostMapping(value = "/info")
    public Result<CanvasDetailResVo> canvasInfo(@RequestBody(required = false) CanvasReqVo canvasReqVo,
                                                HttpServletRequest request) {

        Result<CanvasDetailResVo> result = new Result<>();

        Canvas canvas = canvasService.getById(canvasReqVo.getCanvasId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }

        CanvasDrbStatistics canvasDrbStatistics = canvasDrbStatisticsService.selectLastedByCanvasId(canvas.getId());

        Dao dao = daoService.getById(canvas.getDaoId());

        CanvasDetailResVo canvasDetailResVo = CanvasDetailResVo.transfer(canvas, canvasDrbStatistics, dao);
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(userAddress)) {
            if (canvas.getOwnerAddress().equals(userAddress)) {
                canvasDetailResVo.setModifiable(true);
            }
            Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.CANVAS_FAVORITE.getType(),
                    canvas.getId() + "", userAddress);
            if (favorites != null) {
                canvasDetailResVo.setFavorited(true);
            }
        }
        result.setData(canvasDetailResVo);

        return result;
    }

    /**
     * canvas修改查询详情 1.9
     */
    @PostMapping(value = "/edit/info")
    public Result<CanvasDetailResVo> canvasEditInfo(@RequestBody(required = false) CanvasReqVo canvasReqVo,
                                                    HttpServletRequest request) {

        Result<CanvasDetailResVo> result = new Result<>();

        Canvas canvas = canvasService.getById(canvasReqVo.getCanvasId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress) || !canvas.getOwnerAddress().equals(userAddress)) {
            result.setResultDesc("You are not the owner, please check the connected wallet account.");
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.getById(canvas.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Dao not exist!");
            return result;
        }

        CanvasDetailResVo canvasDetailResVo = CanvasDetailResVo.transfer(canvas, null, dao);

        result.setData(canvasDetailResVo);

        return result;
    }

    /**
     * canvas下未铸造的work列表 1.6
     */
    @PostMapping(value = "/unmintedWorks")
    public Result<WorkListVo> canvasUnmintedWorks(@RequestBody(required = false) CanvasSortedReqVo canvasSortedReqVo,
                                                  HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();
        Canvas canvas = canvasService.getById(canvasSortedReqVo.getCanId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }
        Page<Work> iPage = new Page<>(canvasSortedReqVo.getPageNo(), canvasSortedReqVo.getPageSize());
        canvasSortedReqVo.setCanvasId(canvas.getCanvasId());
        Page<Work> workPage = workService.selectWorkByCanvasId(iPage, canvasSortedReqVo);
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
                works.stream().map(v -> WorkListVo.transfor(v, canvas)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(canvasSortedReqVo.getPageNo());
        page.setPageSize(canvasSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * canvas下nft列表
     */
    @PostMapping(value = "/nfts")
    public Result<WorkListVo> canvasNfts(@RequestBody(required = false) CanvasSortedReqVo canvasSortedReqVo,
                                         HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();
        Canvas canvas = canvasService.getById(canvasSortedReqVo.getCanId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }
        Page<Work> iPage = new Page<>(canvasSortedReqVo.getPageNo(), canvasSortedReqVo.getPageSize());
        canvasSortedReqVo.setCanvasId(canvas.getCanvasId());
        canvasSortedReqVo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        Page<Work> workPage = workService.selectWorkByCanvasId(iPage, canvasSortedReqVo);
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
                works.stream().map(v -> WorkListVo.transfor(v, canvas)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(canvasSortedReqVo.getPageNo());
        page.setPageSize(canvasSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;

    }

    /**
     * canvas当前drb下铸造的nft
     */
    @PostMapping(value = "/drbNfts")
    public Result<WorkListVo> canvasDrbNfts(@RequestBody(required = false) CanvasSortedReqVo canvasSortedReqVo,
                                            HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();
        Canvas canvas = canvasService.getById(canvasSortedReqVo.getCanId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }
        Dao dao = daoService.getById(canvas.getDaoId());
        Page<Work> iPage = new Page<>(canvasSortedReqVo.getPageNo(), canvasSortedReqVo.getPageSize());
        canvasSortedReqVo.setCanvasId(canvas.getCanvasId());
        canvasSortedReqVo.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        canvasSortedReqVo.setCurrentDrb(Integer.valueOf(dao.getCurrentRound()));
        Page<Work> workPage = workService.selectWorkByCanvasId(iPage, canvasSortedReqVo);
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
                works.stream().map(v -> WorkListVo.transfor(v, canvas)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(canvasSortedReqVo.getPageNo());
        page.setPageSize(canvasSortedReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * canvas下Rewards
     */
    @PostMapping(value = "/rewards")
    public Result<CanvasRewardsResVo> canvasRewards(@RequestBody(required = false) CanvasReqVo canvasReqVo) {

        Result<CanvasRewardsResVo> result = new Result<>();

        Canvas canvas = canvasService.getById(canvasReqVo.getCanvasId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }

        Page<CanvasDrbStatistics> iPage = new Page<>(canvasReqVo.getPageNo(), canvasReqVo.getPageSize());
        Page<CanvasDrbStatistics> canvasDrbStatisticsPage =
                canvasDrbStatisticsService.selectByCanvasId(iPage, canvas.getId() + "");

        List<CanvasDrbStatistics> canvasDrbStatistics = canvasDrbStatisticsPage.getRecords();
        List<CanvasRewardsResVo> daoActivityVoList =
                canvasDrbStatistics.stream().map(CanvasRewardsResVo::transfer).collect(Collectors.toList());
        result.setDataList(daoActivityVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(canvasReqVo.getPageNo());
        page.setPageSize(canvasReqVo.getPageSize());
        page.setCount(canvasDrbStatisticsPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * collections下的canvas列表
     */
    @PostMapping(value = "/collections")
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
     * rankings canvas列表
     */
    @PostMapping(value = "/rankings")
    public Result<CanvasRankingsResVo> canvasRankings(@RequestBody(required = false) PageVo pageVo,
                                                      HttpServletRequest request) {

        Result<CanvasRankingsResVo> result = new Result<>();
        Page<CanvasDrbStatistics> iPage = new Page<>(pageVo.getPageNo(), pageVo.getPageSize());
        Page<CanvasDrbStatistics> canvasDrbStatisticsPage = canvasDrbStatisticsService.canvasRanking(iPage);
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
        List<CanvasRankingsResVo> canvasListVoList =
                canvasDrbStatistics.stream().map(CanvasRankingsResVo::transfer).collect(Collectors.toList());
        result.setDataList(canvasListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(pageVo.getPageNo());
        page.setPageSize(pageVo.getPageSize());
        page.setCount(canvasDrbStatisticsPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 创建canvas页面 根据canvas信息换取uri地址 version_1.5 返回的data中为uri地址
     */
    @PostMapping(value = "/create/uri")
    @RepeatSubmit(key = "canvas_create_uri")
    public Result<CanvasCreateResVo> create4Uri(HttpServletRequest request, CanvasCreateReqVo canvasCreateReqVo) {

        Result<CanvasCreateResVo> result = new Result<>();
        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isNotBlank(useraddress)) {
            canvasCreateReqVo.setUserAddress(useraddress);
        } else {
            result.setResultDesc("please login.");
            result.setResultCode(ResultDesc.USER_ERROR.getResultCode());
            return result;
        }

        log.info("[canvas-create-uri] canvasCreateReqVo:{}", JacksonUtil.obj2json(canvasCreateReqVo));
        // check canvas name
        String name = canvasCreateReqVo.getCanvasName().trim();
        String checkString = CommonUtil.nameCheck(name);
        if (StringUtils.isNotBlank(checkString)) {
            result.setResultDesc(checkString);
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Canvas canvasName =
                canvasService.selectCanvasByName(canvasCreateReqVo.getCanvasName(), canvasCreateReqVo.getDaoId());
        if (canvasName != null) {
            result.setResultDesc("Invalid name. The name is already taken.");
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.getById(canvasCreateReqVo.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("dao not exists");
            return result;
        }
        if (canvasCreateReqVo.getCanvasLogo() != null
                && StringUtils.isNotBlank(canvasCreateReqVo.getCanvasLogo().getOriginalFilename())) {
            try {
                MultipartFile multipartFile = canvasCreateReqVo.getCanvasLogo();
                // 文件上传前的名称 处理图片用
                String logoName = multipartFile.getOriginalFilename();
                String imageName = CodeUtil.generateCode('D');
                logoName = imageName + logoName.substring(logoName.lastIndexOf("."));
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.canvasBucketName, multipartFile,
                        imageName);
                String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                canvasCreateReqVo.setCanvasLogoUrl(urlPrefix + ProtoDaoConstant.canvasBucketName + "/" + logoName);
                log.info("[canvas-create] s3 canvas logo url:{}", urlPrefix + logoName);

            } catch (Exception e) {
                log.error("[canvas-create] s3 canvas logo error e", e);
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("please try again later!");
                return result;
            }

        }

        try {
            // 处理uri用
            NewCanvasUriDto newCanvasUriDto = NewCanvasUriDto.transfer(canvasCreateReqVo);
            String sourceString = JacksonUtil.obj2json(newCanvasUriDto);
            byte[] sourceByte = sourceString.getBytes(StandardCharsets.UTF_8);

            long second = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            String fileName = Md5Utils.md5AsBase64(sourceByte) + String.valueOf(second).substring(5) + ".json";
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

        // Canvas canvas = CanvasCreateReqVo.transfer(canvasCreateReqVo, dao);
        // canvasService.save(canvas);

        CanvasCreateResVo canvasCreateResVo = new CanvasCreateResVo();
        canvasCreateResVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        canvasCreateResVo.setCanvasUri(canvasCreateReqVo.getCanvasUri());
        result.setData(canvasCreateResVo);
        return result;

    }

    /**
     * my canvas-返回使用dataList数据
     */
    @PostMapping(value = "/mycanvas")
    public Result<CanvasNameListResVo>
    myCanvas(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<CanvasNameListResVo> result = new Result<>();
        Page<Canvas> iPage = new Page<>(1, 1000);
        Page<Canvas> canvasPage = canvasService.myCanvas(iPage, userProfilePageReqVo.getUserAddress());
        List<Canvas> canvasList = canvasPage.getRecords();

        if (canvasList.size() > 0) {
            List<CanvasNameListResVo> canvasNameListResVos =
                    canvasList.stream().map(CanvasNameListResVo::transfer).collect(Collectors.toList());
            result.setDataList(canvasNameListResVos);
        } else {
            result.setDataList(new ArrayList<>());
        }
        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(canvasPage.getTotal());
        page.setCount(canvasPage.getTotal());
        result.setPage(page);

        return result;
    }

    /**
     * 创建work页面根据选择的dao查询dao下的canvas-返回dataList数据
     */
    @PostMapping(value = "/list/createWork")
    public Result<CanvasNameListResVo> canvas4CreateWork(@RequestBody(required = false) DaoReqVo daoReqVo) {

        Result<CanvasNameListResVo> result = new Result<>();
        log.info("[canvas4CreateWork]daoReqVo:{}", JacksonUtil.obj2json(daoReqVo));
        List<Canvas> canvasList = canvasService.listCanvasByDaoId(daoReqVo.getDaoId());
        if (canvasList.size() > 0) {
            List<CanvasNameListResVo> canvasNameListResVos = canvasList.stream()
                    .filter(v -> StringUtils.isNotBlank(v.getOwnerAddress())
                            && v.getOwnerAddress().toLowerCase().equals(daoReqVo.getUserAddress()))
                    .map(CanvasNameListResVo::transfer).collect(Collectors.toList());
            result.setDataList(canvasNameListResVos);
        } else {
            result.setDataList(new ArrayList<>());
        }

        return result;
    }

    /**
     * 创建work页面选择canvas后，查询返回Unminted Works及DAO Creator Fee等信息 version_1.9 -返回使用data数据
     */
    @PostMapping(value = "/createWork/info")
    public Result<CanvasForCreateWorkResVo>
    createWorkInfo(@RequestBody(required = false) CanvasSortedReqVo canvasSortedReqVo) {

        Result<CanvasForCreateWorkResVo> result = new Result<>();

        Canvas canvas = canvasService.getById(canvasSortedReqVo.getCanId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }
        IPage<Work> workIPage = new Page<>(1, 3);
        canvasSortedReqVo.setCanvasId(canvas.getCanvasId());
        Page<Work> workPage = workService.selectWorkByCanvasId(workIPage, canvasSortedReqVo);
        Dao dao = daoService.getById(canvas.getDaoId());

        CanvasForCreateWorkResVo canvasForCreateWorkResVo = CanvasForCreateWorkResVo.transfer(dao, canvas, workPage);

        result.setData(canvasForCreateWorkResVo);
        return result;
    }

    /**
     * 修改canvas信息 version_1.5
     */
    @PostMapping(value = "/edit")
    @RepeatSubmit(key = "canvas_edit")
    public Result<String> editCanvas(CanvasEditReqVo canvasEditReqVo, HttpServletRequest request) {

        Result<String> result = new Result<>();
        boolean isChanged = false;

        log.info("[canvas-edit] canvasEditReqVo:{}", JacksonUtil.obj2json(canvasEditReqVo));
        Canvas canvas = canvasService.getById(canvasEditReqVo.getCanvasId());
        if (canvas == null) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Canvas is not exist!");
            return result;
        }
        if (DaoStatusEnum.FINISHED.getStatus() == canvas.getDaoStatus()) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("You can no longer edit as the DAO Mint Window has ended.");
            return result;
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(userAddress) || !canvas.getOwnerAddress().equals(userAddress)) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("You are not the owner, please check the connected wallet account.");
            return result;
        }

        if (StringUtils.isBlank(canvasEditReqVo.getCanvasName())) {
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("Name must contain one letter or digit.");
            return result;
        }
        NewCanvasUriDto newCanvasUriDto = restTemplate.getForObject(canvas.getCanvasUri(), NewCanvasUriDto.class);
        if (!canvasEditReqVo.getCanvasName().equals(canvas.getCanvasName())) {
            String name = canvasEditReqVo.getCanvasName().trim();
            String checkString = CommonUtil.nameCheck(name);
            if (StringUtils.isNotBlank(checkString)) {
                result.setResultDesc(checkString);
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                return result;
            }
            Canvas canvasName =
                    canvasService.selectCanvasByName(canvasEditReqVo.getCanvasName(), canvas.getDaoId() + "");
            if (canvasName != null && !canvasName.getId().equals(canvasEditReqVo.getCanvasId())) {
                result.setResultDesc("Invalid name. The name is already taken.");
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                return result;
            }
            newCanvasUriDto.setName(canvasEditReqVo.getCanvasName());
            canvas.setCanvasName(canvasEditReqVo.getCanvasName());
            isChanged = true;
        }
        if (StringUtils.isNotBlank(canvasEditReqVo.getCanvasDescription())
                && !canvasEditReqVo.getCanvasDescription().equals(canvas.getCanvasDescription())) {
            newCanvasUriDto.setDescription(canvasEditReqVo.getCanvasDescription());
            canvas.setCanvasDescription(canvasEditReqVo.getCanvasDescription());
            isChanged = true;
        }

        if (canvasEditReqVo.getCanvasLogo() != null
                && StringUtils.isNotBlank(canvasEditReqVo.getCanvasLogo().getOriginalFilename())) {
            try {
                isChanged = true;
                MultipartFile multipartFile = canvasEditReqVo.getCanvasLogo();
                // 文件上传前的名称 处理图片用
                String logoName = multipartFile.getOriginalFilename();
                String imageName = CodeUtil.generateCode('D');
                logoName = imageName + logoName.substring(logoName.lastIndexOf("."));
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.canvasBucketName, multipartFile,
                        imageName);
                String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
                canvasEditReqVo.setCanvasLogoUrl(urlPrefix + ProtoDaoConstant.canvasBucketName + "/" + logoName);
                log.info("[canvas-edit] s3 canvas logo url:{}", urlPrefix + logoName);
                newCanvasUriDto.setLogo(canvasEditReqVo.getCanvasLogoUrl());
                canvas.setCanvasLogo(canvasEditReqVo.getCanvasLogoUrl());
            } catch (Exception e) {
                log.error("[canvas-edit] s3 canvas logo error e", e);
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("please try again later!");
                return result;
            }

        }

        try {
            // 处理uri用
            if (isChanged) {
                String fileName = canvas.getCanvasUri().substring(canvas.getCanvasUri().lastIndexOf("/") + 1);

                s3Service.deleteObject(
                        ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.canvasBucketName,
                        fileName);

                BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
                representaion.setObjectName(fileName);
                representaion.setText(JacksonUtil.obj2json(newCanvasUriDto));
                s3Service.putObject(
                        ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.canvasBucketName,
                        representaion);

                log.info("[canvas-edit] canvasUri:{}", canvas.getCanvasUri());

            }

        } catch (Exception e) {
            log.error("[canvas-edit] upload newCanvasUriDto error param:{} e:{}", JacksonUtil.obj2json(canvasEditReqVo),
                    e);
            result.setResultDesc("network error please try again later.");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        canvas.setDiscordLink(canvasEditReqVo.getDiscordLink());
        canvas.setOpenseaLink(canvasEditReqVo.getOpenseaLink());
        canvas.setTwitterLink(canvasEditReqVo.getTwitterLink());
        canvas.setSocialLinks(canvasEditReqVo.getSocialLinks());

        canvasService.updateById(canvas);

        return result;
    }

    /**
     * 获取canvas铸造费用 1.6
     */
    @PostMapping(value = "/create/fee")
    public Result<Double> canvasCreateFee(HttpServletRequest request) {

        Result<Double> result = new Result<>();

        if (ProtoDaoConstant.CREATE_CANVAS_FEE == null) {
            result.setResultDesc("system exception, please try again later!");
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            return result;
        }

        result.setData(new BigDecimal(String.valueOf(ProtoDaoConstant.CREATE_CANVAS_FEE))
                .divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 4, RoundingMode.FLOOR).doubleValue());

        return result;
    }

    /**
     * canvas修改链上信息查询 1.9
     */
    @PostMapping(value = "/edit/chain")
    public Result<CanvasBenefitsDistributeResVo> canvasEditChain(@RequestBody(required = false) CanvasReqVo canvasReqVo,
                                                                 HttpServletRequest request) {
        Result<CanvasBenefitsDistributeResVo> result = new Result<>();
        if (canvasReqVo == null || StringUtils.isBlank(canvasReqVo.getCanvasId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Canvas canvas = canvasService.getById(canvasReqVo.getCanvasId());
        if (canvas == null || !canvas.getOwnerAddress().equals(canvasReqVo.getUserAddress())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }

        Dao dao = daoService.daoDetailByDaoId(canvas.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("Dao is not exist!");
            return result;
        }
        // 不支持修改的情况
        CanvasBenefitsDistributeResVo canvasBenefitsDistributeResVo = new CanvasBenefitsDistributeResVo();
        canvasBenefitsDistributeResVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        canvasBenefitsDistributeResVo.setCanvasId(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
        canvasBenefitsDistributeResVo.setDaoVersion(dao.getDaoVersion());
        if (dao.getDaoVersion() >= 2) {
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
            if (dao.getDaoVersion() == 2) {
                daoRoyaltyToken.setCanvasReward(
                        ProtoDaoCommonUtil.bigdecimalPercentageToString(daoRoyaltyToken.getCanvasReward(), true));
                daoRoyaltyToken.setMinterReward(
                        ProtoDaoCommonUtil.bigdecimalPercentageToString(daoRoyaltyToken.getMinterReward(), false));
            } else {
                daoRoyaltyToken.setCanvasReward(daoRoyaltyToken.getCanvasReward());
                daoRoyaltyToken.setMinterReward(daoRoyaltyToken.getMinterReward());
            }

            canvasBenefitsDistributeResVo.setDaoRoyaltyToken(daoRoyaltyToken);
            canvasBenefitsDistributeResVo.setFixedReserveRatio(fixedReserveRatio);
            canvasBenefitsDistributeResVo.setUnFixedReserveRatio(unFixedReserveRatio);
        }

        canvasBenefitsDistributeResVo.setExtraRoyaltyToken(canvas.getRoyaltyToken());

        result.setData(canvasBenefitsDistributeResVo);
        return result;
    }
}
