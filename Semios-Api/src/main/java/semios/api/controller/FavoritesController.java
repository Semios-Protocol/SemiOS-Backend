package semios.api.controller;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.annotation.RepeatSubmit;
import semios.api.model.dto.common.Result;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Favorites;
import semios.api.model.entity.Work;
import semios.api.model.vo.req.DaoInfo.DaoInfoVo;
import semios.api.model.vo.req.FavoriteReqVo;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.req.WorkInfo.WorkInfo;
import semios.api.model.vo.res.CanvasListResVo;
import semios.api.model.vo.res.WorkListVoV2;
import semios.api.service.*;
import semios.api.service.common.CommonService;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 收藏相关接口
 *
 * @author xiangbin
 * @order 5
 */
@Slf4j
@RestController
@RequestMapping("/favorite")
public class FavoritesController {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IWorkService workService;

    @Autowired
    private IFavoritesService favoritesService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    /**
     * 收藏列表-dao
     */
    @PostMapping(value = "/dao")
    public Result<DaoInfoVo> daoFavorite(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<DaoInfoVo> result = new Result<>();
        String userAddress = userProfilePageReqVo.getUserAddress();
        Page<Dao> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Dao> daoPage = daoService.findFavoritesByUserAddress(iPage, userAddress);
        List<Dao> daoList = daoPage.getRecords();

        List<Integer> favoritesIds = daoList.stream().map(Dao::getId).collect(Collectors.toList());

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
     * 收藏列表-canvas
     */
    @PostMapping(value = "/canvas")
    public Result<CanvasListResVo>
    canvasFavorite(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<CanvasListResVo> result = new Result<>();
        String userAddress = userProfilePageReqVo.getUserAddress();

        Page<Canvas> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Canvas> canvasPage = canvasService.findFavoritesByUserAddress(iPage, userAddress);
        List<Canvas> canvasList = canvasPage.getRecords();
        List<CanvasListResVo> canvasListResVoList = canvasList.stream().map(v -> {
            List<Work> workList = workService.selectWorksForCanvasPic(v.getId() + "");
            return CanvasListResVo.transfer(v, workList, null, true);
        }).collect(Collectors.toList());
        result.setDataList(canvasListResVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(canvasPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 收藏列表-works
     */
    @PostMapping(value = "/work")
    public Result<WorkInfo> workFavorite(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {

        Result<WorkInfo> result = new Result<>();
        String userAddress = userProfilePageReqVo.getUserAddress();

        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Work> workPage = workService.findFavoritesByUserAddress(iPage, userAddress);
        List<Work> works = workPage.getRecords();
        List<Integer> favoritesIds = works.stream().map(Work::getId).collect(Collectors.toList());

        List<WorkInfo> workListVos =
                works.stream().map(v -> WorkInfo.transfer(v, favoritesIds)).collect(Collectors.toList());
        result.setDataList(workListVos);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 1.6.1 收藏列表-works(图片形式) P1
     */
    @PostMapping(value = "/work/v2")
    public Result<WorkListVoV2> workFavoriteV2(@RequestBody(required = false) UserProfilePageReqVo userProfilePageReqVo) {
        Result<WorkListVoV2> result = new Result<>();
        String userAddress = userProfilePageReqVo.getUserAddress();

        Page<Work> iPage = new Page<>(userProfilePageReqVo.getPageNo(), userProfilePageReqVo.getPageSize());
        Page<Work> workPage = workService.findFavoritesByUserAddress(iPage, userAddress);
        List<Work> workList = workPage.getRecords();
        workList.forEach(v -> v.setFavorited(true));

        List<WorkListVoV2> workListVoList =
                workList.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(userProfilePageReqVo.getPageNo());
        page.setPageSize(userProfilePageReqVo.getPageSize());
        page.setCount(workPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * 收藏按钮点击 resulteCode 100收藏成功
     */
    @PostMapping(value = "/actions")
    @RepeatSubmit(key = "favorite_actions")
    public Result<Boolean> favoriteActions(@RequestBody(required = false) FavoriteReqVo favoriteReqVo) {

        Result<Boolean> result = new Result<>();

        Favorites favorites = new Favorites();
        String userAddress = favoriteReqVo.getUserAddress();
        favorites.setUserAddress(userAddress);
        favorites.setCreateTime(LocalDateTime.now());
        favorites.setType(favoriteReqVo.getType());
        favorites.setFavoriteId(favoriteReqVo.getFavoriteId() + "");

        favoritesService.saveFavoriteByType(favorites);

        return result;
    }

    /**
     * 取消收藏 resulteCode 100取消收藏成功
     */
    @PostMapping(value = "/cancel")
    @RepeatSubmit(key = "favorite_cancel")
    public Result<Boolean> favoriteCancel(@RequestBody(required = false) FavoriteReqVo favoriteReqVo) {

        Result<Boolean> result = new Result<>();

        Favorites favorites = new Favorites();
        String userAddress = favoriteReqVo.getUserAddress();
        favorites.setUserAddress(userAddress);
        favorites.setType(favoriteReqVo.getType());
        favorites.setFavoriteId(favoriteReqVo.getFavoriteId() + "");

        favoritesService.removeFavoriteByType(favorites);
        return result;
    }

}
