package semios.api.controller;

import com.google.common.util.concurrent.ThreadFactoryBuilder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.response.SearchNameParamDto;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Favorites;
import semios.api.model.entity.Work;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.res.*;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.CookieUtil;
import semios.api.utils.JacksonUtil;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * 搜索相关接口
 *
 * @order 7
 * @author: xiangbin
 * @create: 2022-08-05 09:59
 **/
@Slf4j
@RestController
@RequestMapping("/search")
public class SearchController {

    // https://www.cnblogs.com/lyhc/p/16661650.html
    public static final ThreadPoolExecutor searchExecutor =
            new ThreadPoolExecutor(4, 4, 10, TimeUnit.SECONDS, new ArrayBlockingQueue<>(50, true),
                    new ThreadFactoryBuilder().setNameFormat("SearchController-searchType-%d").setDaemon(true).build());
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private IWorkService workService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private IFavoritesService favoritesService;
    @Autowired
    private CommonService commonService;
    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    public static void main(String[] args) {
        String name = "D4A@42/Canvas*1230/NFT#12";
        // Pattern pattern = Pattern.compile("^D4A@[1-9][0-9]*/Canvas\\*[1-9][0-9]*/NFT#[1-9][0-9]*$");
        // Matcher matcher = pattern.matcher(name);
        // if(matcher.matches()){

        System.out.println((Long.valueOf(name.substring(4, name.indexOf("/Canvas*")))));
        System.out.println(Long.valueOf(name.substring(name.indexOf("/Canvas*") + 8, name.indexOf("/NFT#"))));
        System.out.println(Long.valueOf(name.substring(name.indexOf("/NFT#") + 5)));

        name = "D4A@42/Canvas*1230";
        System.out.println((Long.valueOf(name.substring(4, name.indexOf("/Canvas*")))));
        System.out.println(Long.valueOf(name.substring(name.indexOf("/Canvas*") + 8)));

        name = "D4A@42";
        System.out.println((Long.valueOf(name.substring(4))));

    }

    /**
     * 1.6.1 搜索返回查询数量
     */
    @PostMapping(value = "/amount")
    public Result<SearchNumberResVo> searchNubmer(@RequestBody(required = false) SearchReqVo searchReqVo) {
        log.info("[searchNubmer]param:{}", JacksonUtil.obj2json(searchReqVo));
        Result<SearchNumberResVo> result = new Result<>();
        SearchNumberResVo searchNumberResVo = new SearchNumberResVo();
        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setData(searchNumberResVo);
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchNubmer]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setData(searchNumberResVo);
            return result;
        }

        try {
            CompletableFuture<Integer> daoFuture = CompletableFuture.supplyAsync(() -> {
                Integer daoAmount = daoService.searchAmount(searchReqVo.getSearchWord());
                return daoAmount;
            }, searchExecutor);

            CompletableFuture<Integer> workFuture = CompletableFuture.supplyAsync(() -> {
                Integer workAmount = workService.searchAmount(searchReqVo.getSearchWord());
                return workAmount;
            }, searchExecutor);

            CompletableFuture<Integer> seedNodesFuture = CompletableFuture.supplyAsync(() -> {
                Integer seedNodeAmount = daoService.searchSeedNodesAmount(searchReqVo.getSearchWord());
                return seedNodeAmount;
            }, searchExecutor);

            CompletableFuture.allOf(daoFuture, workFuture, seedNodesFuture);

            if (daoFuture.get() != null) {
                searchNumberResVo.setDaoAmount(daoFuture.get());
            }
            if (workFuture.get() != null) {
                searchNumberResVo.setWorkAmount(workFuture.get());
            }
            if (seedNodesFuture.get() != null) {
                searchNumberResVo.setSeedNodesAmount(seedNodesFuture.get());
            }
        } catch (InterruptedException e) {
            log.error("[searchNubmer]InterruptedException:", e);
        } catch (ExecutionException e) {
            log.error("[searchNubmer]ExecutionException:", e);
        }

        result.setData(searchNumberResVo);
        return result;
    }

    /**
     * 搜索返回work结果
     */
    @PostMapping(value = "/works")
    public Result<WorkListVo> searchWorkResult(@RequestBody(required = false) SearchReqVo searchReqVo,
                                               HttpServletRequest request) {

        Result<WorkListVo> result = new Result<>();
        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchWorkResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            return result;
        }
        List<Work> workList = new ArrayList<>();
        if (patternWork(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = patternName(searchReqVo.getSearchWord());
            Work work = workService.selectByNumber(searchNameParamDto.getDaoNumber(),
                    searchNameParamDto.getCnavasNumber(), searchNameParamDto.getWorkNumber());
            if (work != null) {
                workList.add(work);
            }
        } else {
//            if (patternDao(searchReqVo.getSearchWord()) || patternCanvas(searchReqVo.getSearchWord())) {
            if (patternDao(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                return result;
            } else {
                workList = workService.searchWork(searchReqVo.getSearchWord());
            }
        }

        if (workList.size() == 0) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (searchReqVo.getNumber() != null && workList.size() > searchReqVo.getNumber()) {
            workList = workList.subList(0, searchReqVo.getNumber());
        }

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
            workList.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVo> workListVoList =
                workList.stream().map(v -> WorkListVo.transfor(v, null)).collect(Collectors.toList());
        result.setDataList(workListVoList);
        return result;
    }

    /**
     * 1.6.1 搜索返回work结果(图片形式) P1
     */
    @PostMapping(value = "/works/v2")
    public Result<WorkListVoV2> searchWorkResultV2(@RequestBody(required = false) SearchReqVo searchReqVo,
                                                   HttpServletRequest request) {
        Result<WorkListVoV2> result = new Result<>();
        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchWorkResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            return result;
        }
        List<Work> workList = new ArrayList<>();

        if (patternWork(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = patternName(searchReqVo.getSearchWord());
            Work work = workService.selectByNumber(searchNameParamDto.getDaoNumber(),
                    searchNameParamDto.getCnavasNumber(), searchNameParamDto.getWorkNumber());
            if (work != null) {
                workList.add(work);
            }
        } else {
            if (patternDao(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                return result;
            } else {
                workList = workService.searchWork(searchReqVo.getSearchWord());
            }
        }

        if (workList.isEmpty()) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (searchReqVo.getNumber() != null && workList.size() > searchReqVo.getNumber()) {
            workList = workList.subList(0, searchReqVo.getNumber());
        }

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
            workList.forEach(v -> v.setFavorited(favoritesIds2.contains(v.getId())));
        }
        List<WorkListVoV2> workListVoList =
                workList.stream().map(v -> WorkListVoV2.transfor(v, null)).collect(Collectors.toList());

        result.setDataList(workListVoList);

        return result;
    }

    /**
     * 搜索返回canvas结果
     */
    @PostMapping(value = "/canvas")
    public Result<CanvasListResVo> searchCanvasResult(@RequestBody(required = false) SearchReqVo searchReqVo,
                                                      HttpServletRequest request) {

        Result<CanvasListResVo> result = new Result<>();
        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchCanvasResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            return result;
        }
        List<Canvas> canvasList = new ArrayList<>();

        if (patternCanvas(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = patternName(searchReqVo.getSearchWord());
            Canvas canvas =
                    canvasService.selectByNumber(searchNameParamDto.getDaoNumber(), searchNameParamDto.getCnavasNumber());
            if (canvas != null) {
                canvasList.add(canvas);
            }
        } else {
            if (patternDao(searchReqVo.getSearchWord()) || patternWork(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                return result;
            }
            canvasList = canvasService.searchCanvas(searchReqVo.getSearchWord());
        }
        if (canvasList.size() == 0) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (searchReqVo.getNumber() != null && canvasList.size() > searchReqVo.getNumber()) {
            canvasList = canvasList.subList(0, searchReqVo.getNumber());
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

        List<CanvasListResVo> canvasListResVoList = canvasList.stream().map(v -> {
            List<Work> workList = workService.selectWorksForCanvasPic(v.getId() + "");
            return CanvasListResVo.transfer(v, workList, userAddress, false);
        }).collect(Collectors.toList());

        result.setDataList(canvasListResVoList);
        return result;
    }

    /**
     * 搜索返回dao结果
     */
    @PostMapping(value = "/daos")
    public Result<TogetherDaoListVo> searchDaoResult(@RequestBody(required = false) SearchReqVo searchReqVo,
                                                     HttpServletRequest request) {
        // 返回值 DaoListVo 修改为 TogetherDaoListVo
        Result<TogetherDaoListVo> result = new Result<>();
        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchDaoResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            return result;
        }
        List<Dao> daoList = new ArrayList<>();
        if (patternDao(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = patternName(searchReqVo.getSearchWord());
            Dao dao = daoService.selectDaoByDaoNumber(searchNameParamDto.getDaoNumber().intValue());
            if (dao != null) {
                daoList.add(dao);
            }
        } else {
//            if (patternWork(searchReqVo.getSearchWord()) || patternCanvas(searchReqVo.getSearchWord())) {
            if (patternWork(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                return result;
            }
            daoList = daoService.searchDao(searchReqVo.getSearchWord());
        }

        if (daoList.size() == 0) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (searchReqVo.getNumber() != null && daoList.size() > searchReqVo.getNumber()) {
            daoList = daoList.subList(0, searchReqVo.getNumber());
        }
        String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);

        List<DaoListVo> daoListVoList =
                daoList.stream().map(v -> DaoListVo.transfer(v, userAddress, false)).collect(Collectors.toList());
        log.info("[SearchController]--原来的值:" + JacksonUtil.obj2json(daoListVoList));
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
            //List<TogetherDaoListVo> togetherDaoListVoList = daoListTo.stream().map(v-> TogetherDaoListVo.transferTogetherDaoListVo(v,userAddress)).collect(Collectors.toList());
            result.setDataList(togetherDaoListVoList);
        }

        return result;
    }

    /**
     * 1.6.1 搜索返回seedNodes结果
     */
    @PostMapping(value = "/seedNodes")
    public Result<SeedNodesListVo> searchSeedNodesResult(@RequestBody(required = false) SearchReqVo searchReqVo,
                                                         HttpServletRequest request) {
        Result<SeedNodesListVo> result = new Result<>();
        if (searchReqVo == null || StringUtils.isBlank(searchReqVo.getSearchWord())) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (CommonUtil.isContainChinese(searchReqVo.getSearchWord())) {
            log.info("[searchDaoResult]containChinese searchWord:{}", searchReqVo.getSearchWord());
            result.setDataList(new ArrayList<>());
            return result;
        }
        List<Dao> daoList = new ArrayList<>();
        if (patternDao(searchReqVo.getSearchWord())) {
            SearchNameParamDto searchNameParamDto = patternName(searchReqVo.getSearchWord());
            Dao dao = daoService.selectSeedNodesByDaoNumber(searchNameParamDto.getDaoNumber().intValue());
            if (dao != null) {
                daoList.add(dao);
            }
        } else {
            if (patternWork(searchReqVo.getSearchWord())) {
                result.setDataList(new ArrayList<>());
                return result;
            }
            daoList = daoService.searchSeedNodes(searchReqVo.getSearchWord());
        }

        if (daoList.isEmpty()) {
            result.setDataList(new ArrayList<>());
            return result;
        }
        if (searchReqVo.getNumber() != null && daoList.size() > searchReqVo.getNumber()) {
            daoList = daoList.subList(0, searchReqVo.getNumber());
        }
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

        return result;
    }

    /**
     * 获取daoNumber canvasNumber workNumber
     *
     * @param name
     * @return
     */
    public SearchNameParamDto patternName(String name) {
        SearchNameParamDto searchNameParamDto = new SearchNameParamDto();
        if (StringUtils.isBlank(name)) {
            return searchNameParamDto;
        }
        name = name.replaceAll(" ", "");
        if (!name.startsWith("D4A@")) {
            return searchNameParamDto;
        }

        if (patternWork(name)) {
            searchNameParamDto.setDaoNumber(Long.valueOf(name.substring(4, name.indexOf("/Canvas*"))));
            searchNameParamDto
                    .setCnavasNumber(Long.valueOf(name.substring(name.indexOf("/Canvas*") + 8, name.indexOf("/NFT#"))));
            searchNameParamDto.setWorkNumber(Long.valueOf(name.substring(name.indexOf("/NFT#") + 5)));
            return searchNameParamDto;
        }
//        if (patternCanvas(name)) {
//            searchNameParamDto.setDaoNumber(Long.valueOf(name.substring(4, name.indexOf("/Canvas*"))));
//            searchNameParamDto.setCnavasNumber(Long.valueOf(name.substring(name.indexOf("/Canvas*") + 8)));
//            return searchNameParamDto;
//        }

        if (patternDao(name)) {
            searchNameParamDto.setDaoNumber(Long.valueOf(name.substring(4)));
            return searchNameParamDto;
        }

        return searchNameParamDto;
    }

    /**
     * 是否匹配 D4A@123
     *
     * @param name
     * @return
     */
    public boolean patternDao(String name) {
        String dao = "^PDAO.T[1-9][0-9]*$";
        Pattern pattern = Pattern.compile(dao);
        Matcher matcher = pattern.matcher(name);
        return matcher.matches();
    }

    /**
     * 是否匹配 D4A@123/Canvas*234
     *
     * @param name
     * @return
     */
    public boolean patternCanvas(String name) {
        String canvas = "^D4A@[1-9][0-9]*/Canvas\\*[1-9][0-9]*$";
        Pattern pattern = Pattern.compile(canvas);
        Matcher matcher = pattern.matcher(name);
        return matcher.matches();
    }

    /**
     * 是否匹配 D4A@123/Canvas*234/NFT#123
     *
     * @param name
     * @return
     */
    public boolean patternWork(String name) {
        String work = "^PDAO\\.T[1-9][0-9]*\\.[1-9][0-9]*$";
        Pattern pattern = Pattern.compile(work);
        Matcher matcher = pattern.matcher(name);
        return matcher.matches();
    }

}
