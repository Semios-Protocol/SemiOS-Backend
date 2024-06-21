package semios.version1_6_1;

// 1.6的需求

import lombok.extern.slf4j.Slf4j;
import org.junit.Assert;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.vo.res.*;
import semios.api.SemiosApiApplication;
import semios.api.controller.DaoController;
import semios.api.controller.FavoritesController;
import semios.api.controller.SearchController;
import semios.api.controller.WorkController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.vo.req.DaoIdReqVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import java.util.List;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class SemiosTest {


    private final String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";

    @Resource
    WorkController workController;

    @Resource
    DaoController daoController;

    @Resource
    SearchController searchController;

    @Resource
    FavoritesController favoritesController;

    // 模仿session
    private static MockHttpSession session = new MockHttpSession();
    // 模仿request
    private static MockHttpServletRequest request = new MockHttpServletRequest();
    // 模仿response
    private static MockHttpServletResponse response = new MockHttpServletResponse();

    @Test
    @Order(0)
    void  startApplication(){
        SpringBeanUtil.applicationContext = SpringApplication.run(SemiosApiApplication.class);
    }

    // WorkController include
    @Test
    @Order(1)
    void exploreUnmintedWorksV2(){
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);

        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        returnVo =  workController.exploreUnmintedWorksV2(daoSortedReqVo,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = workController.exploreUnmintedWorks(daoSortedReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));

        // have user address
        returnVo =  workController.exploreUnmintedWorksV2(daoSortedReqVo,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());
        // check  the distinction
        returnV1 = workController.exploreUnmintedWorks(daoSortedReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    @Test
    @Order(2)
    void exploreNftsV2(){
        // SpringBeanUtil.applicationContext = SpringApplication.run(ProtoDaoApplication.class);

        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);

        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;


        returnVo =  workController.exploreNftsV2(daoSortedReqVo,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = workController.exploreNfts(daoSortedReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));


        // check hava user address
        returnVo =  workController.exploreNftsV2(daoSortedReqVo,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = workController.exploreNfts(daoSortedReqVo,addressRequest(address));
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    @Test
    @Order(3)
    void workHoleV2(){
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        userProfilePageReqVo.setPageNo(1L);
        userProfilePageReqVo.setPageSize(10L);
        userProfilePageReqVo.setUserAddress(address);
        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        returnVo =  workController.workHoleV2(userProfilePageReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = workController.workHole(userProfilePageReqVo);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    @Test
    @Order(4)
    void workMintedV2() {
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        userProfilePageReqVo.setPageNo(1L);
        userProfilePageReqVo.setPageSize(10L);
        userProfilePageReqVo.setUserAddress(address);
        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        returnVo =  workController.workMintedV2(userProfilePageReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = workController.workMinted(userProfilePageReqVo);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    @Test
    @Order(5)
    void workCreatorV2() {
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        userProfilePageReqVo.setPageNo(1L);
        userProfilePageReqVo.setPageSize(10L);
        userProfilePageReqVo.setUserAddress(address);
        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        returnVo =  workController.workCreatorV2(userProfilePageReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = workController.workCreator(userProfilePageReqVo);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }


    // DaoController include
    @Test
    @Order(10)
    void daoNftsV2(){
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);


        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        // have not dao id
        returnVo = daoController.daoNftsV2(daoSortedReqVo,request);
        Assert.assertEquals("没有daoID情况",ResultDesc.FAIL.getResultCode(),returnVo.getResultCode());

        // success ..
        daoSortedReqVo.setDaoId("76");
        returnVo = daoController.daoNftsV2(daoSortedReqVo,request);
        // check  the distinction
        returnV1 = daoController.daoNfts(daoSortedReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));


        // check hava user address
        returnVo =  daoController.daoNftsV2(daoSortedReqVo,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = daoController.daoNfts(daoSortedReqVo,addressRequest(address));
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    @Test
    @Order(11)
    void daoUnmintedWorksV2(){
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);

        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        // have not dao id
        returnVo = daoController.daoUnmintedWorksV2(daoSortedReqVo,request);
        Assert.assertEquals("没有daoID情况",ResultDesc.FAIL.getResultCode(),returnVo.getResultCode());

        // success ..
        daoSortedReqVo.setDaoId("76");

        returnVo = daoController.daoUnmintedWorksV2(daoSortedReqVo,request);
        // check  the distinction
        returnV1 = daoController.daoUnmintedWorks(daoSortedReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));


        // check hava user address
        returnVo =  daoController.daoUnmintedWorksV2(daoSortedReqVo,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = daoController.daoUnmintedWorks(daoSortedReqVo,addressRequest(address));
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    @Test
    @Order(12)
    void daoDrbNftsV2(){
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);

        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        // have not dao id
        returnVo = daoController.daoDrbNftsV2(daoSortedReqVo,request);
        Assert.assertEquals("没有daoID情况",ResultDesc.FAIL.getResultCode(),returnVo.getResultCode());

        // success ..
        daoSortedReqVo.setDaoId("76");

        returnVo = daoController.daoDrbNftsV2(daoSortedReqVo,request);
        // check  the distinction
        returnV1 = daoController.daoDrbNfts(daoSortedReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));


        // check hava user address
        returnVo =  daoController.daoDrbNftsV2(daoSortedReqVo,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = daoController.daoDrbNfts(daoSortedReqVo,addressRequest(address));
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }


    @Test
    @Order(13)
    void daoExploreSeedNodes(){
        // SpringBeanUtil.applicationContext = SpringApplication.run(ProtoDaoApplication.class);
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageSize(10L);
        daoSortedReqVo.setPageNo(1L);

        Result<SeedNodesListVo> returnVo;

        returnVo = daoController.daoExploreSeedNodes(daoSortedReqVo,request);
        log.info("returnVo:"+JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("接口返回",ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    void daoBlackAndWhiteList(){
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        daoIdReqVo.setDaoId("186");
        daoIdReqVo.setUserAddress("0x125d349a706f7fe6790ac73b1c32e000a6919b12");

        Result<DaoWhiteListResVo> returnVo;

        returnVo = daoController.daoBlackAndWhiteList(daoIdReqVo,request);
        log.info("returnVo:"+JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("接口返回",ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }



    // SearchController
    @Test
    @Order(20)
    void searchNubmer(){
        SearchReqVo searchReqVo = new SearchReqVo();
        searchReqVo.setSearchWord("11");
        Result<SearchNumberResVo> returnVo;

        returnVo = searchController.searchNubmer(searchReqVo);
        log.info("returnVo:"+JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("没有daoID情况",ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    @Order(21)
    void searchWorkResultV2(){
        SearchReqVo searchReqVo = new SearchReqVo();

        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        // have not anything
        returnVo = searchController.searchWorkResultV2(searchReqVo,request);
        Assert.assertEquals("没有输入情况",ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertTrue("成功", returnVo.getDataList() == null || returnVo.getDataList().isEmpty());


        // search result by work = 11
        searchReqVo.setSearchWord("11");

        returnVo = searchController.searchWorkResultV2(searchReqVo,request);
        // check  the distinction
        returnV1 = searchController.searchWorkResult(searchReqVo,request);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));


        // check hava user address
        returnVo =  searchController.searchWorkResultV2(searchReqVo,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = searchController.searchWorkResult(searchReqVo,addressRequest(address));
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }


    @Test
    @Order(22)
    void searchSeedNodesResult(){
        SearchReqVo searchReqVo = new SearchReqVo();

        Result<SeedNodesListVo> returnVo;

        // have not anything
        returnVo = searchController.searchSeedNodesResult(searchReqVo,request);
        Assert.assertEquals("没有输入情况",ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertTrue("成功", returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // search result by work = 11
        searchReqVo.setSearchWord("11");
        returnVo = searchController.searchSeedNodesResult(searchReqVo,request);
        log.info("returnVo:"+JacksonUtil.obj2json(returnVo));
        Assert.assertNotNull("成功", returnVo.getDataList());

        // check hava user address
        returnVo =  searchController.searchSeedNodesResult(searchReqVo,addressRequest(address));
        log.info("returnVo:"+JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());
    }


    // FavoriteController include..
    @Test
    @Order(30)
    void  workFavoriteV2(){
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        userProfilePageReqVo.setPageNo(1L);
        userProfilePageReqVo.setPageSize(10L);
        userProfilePageReqVo.setUserAddress(address);
        Result<WorkListVoV2> returnVo;
        Result<WorkListVo> returnV1;

        returnVo =  favoritesController.workFavoriteV2(userProfilePageReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());

        // check  the distinction
        returnV1 = favoritesController.workFavorite(userProfilePageReqVo);
        Assert.assertTrue("成功", returnV1.getDataList() != null || !returnV1.getDataList().isEmpty());
        Assert.assertTrue("校验是否一致",checkDistinction(returnV1.getDataList(),returnVo.getDataList()));
    }

    private Boolean checkDistinction(List<WorkListVo> list1, List<WorkListVoV2> list2){

        if (list1.size() != list2.size()) {
            System.out.println("列表长度不相等");
            return false;
        }

        // 逐个比较列表中的元素属性
        for (int i = 0; i < list1.size(); i++) {
            WorkListVo dto1 = list1.get(i);
            WorkListVoV2 dto2 = list2.get(i);
            // 检查相同位置的元素的属性是否相等
            if (!dto2.getWorkId().equals(dto1.getWorkId())){
                return false;
            }
            if (!dto2.getPrice().equals(dto1.getPrice())){
                return false;
            }
            if(!dto2.getFavoriteAmount().equals(dto1.getFavoriteAmount())){
                return false;
            }
            if (!dto2.getFavorited().equals(dto1.getFavorited())){
                return false;
            }
        }

        return true;
    }

    private MockHttpServletRequest addressRequest(String address){
        Cookie cookies;
        request = new MockHttpServletRequest();
        request.setCharacterEncoding("UTF-8");

        cookies= new Cookie(ProtoDaoConstant.COOKIE_ADDRESS,address);
        request.setCookies(cookies);
        return request;
    }
}