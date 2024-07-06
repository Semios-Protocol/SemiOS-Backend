package semios.api.controller;

import org.junit.Assert;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.DaoInfo.DaoInfoVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.res.TogetherDaoListVo;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
class DaoCollectionControllerTest {

    private static final Logger log = LoggerFactory.getLogger(DaoCollectionControllerTest.class);
    private final String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";

    @Resource
    DaoCollectionController daoCollectionController;

    @Resource
    DaoController daoController ;

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


    @Test
    @Order(1)
    void daoCollectionsNew() {
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);
        daoSortedReqVo.setSortCondition("0");

        ResultList<DaoInfoVo> returnVo;
        returnVo = daoCollectionController.daoCollections(daoSortedReqVo,addressRequest(address));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
    }

    @Test
    @Order(2)
    void daoCollectionsOld() {
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        daoSortedReqVo.setPageNo(1L);
        daoSortedReqVo.setPageSize(10L);
        daoSortedReqVo.setSortCondition("0");

        Result<TogetherDaoListVo> returnVo;
        returnVo = daoController.daoCollections(daoSortedReqVo,addressRequest(address));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
    }


//    @Test
//    @Order(2)
//    void mintWindowInfo() {
//        DaoIdParam daoIdParam = new DaoIdParam();
//        daoIdParam.setDaoId("100");
//
//        Result<MintWindowInfoVo> returnVo;
//        returnVo = daoCollectionController.mintWindowInfo(daoIdParam,request);
//        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
//        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
//    }
//
//    @Test
//    @Order(3)
//    void daoBasicInfo() {
//        DaoIdParam daoIdParam = new DaoIdParam();
//        daoIdParam.setDaoId("100");
//
//        Result<BasicInformationVo> returnVo;
//        returnVo = daoCollectionController.daoBasicInfo(daoIdParam,request);
//        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
//        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
//    }
//
//
//    @Test
//    @Order(4)
//    void modeStatus() {
//        DaoIdParam daoIdParam = new DaoIdParam();
//        daoIdParam.setDaoId("100");
//
//        Result<ModeStatusVo> returnVo;
//        returnVo = daoCollectionController.modeStatus(daoIdParam,request);
//        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
//        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
//    }
//
//    @Test
//    @Order(5)
//    void daoListByTogetherId() {
//        startApplication();
//        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
//        daoSortedReqVo.setDaoId("331");
//        daoSortedReqVo.setPageNo(1L);
//        daoSortedReqVo.setPageSize(10L);
//
//        ResultList<DaoInfoVo> returnVo;
//        returnVo = daoCollectionController.daoListByTogetherId(daoSortedReqVo,request);
//        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
//        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
//    }
//
//    @Test
//    @Order(5)
//    void searchDao() {
//        startApplication();
//        SearchReqVo searchReqVo = new SearchReqVo();
//        searchReqVo.setSearchWord("E");
//
//        Result<DaoInfoVo> returnVo;
//        returnVo = daoCollectionController.searchDao(searchReqVo,new MockHttpServletRequest());
//        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
//        log.info("return vo:{}", JacksonUtil.obj2json(returnVo));
//    }



    private MockHttpServletRequest addressRequest(String address){
        Cookie cookies;
        request = new MockHttpServletRequest();
        request.setCharacterEncoding("UTF-8");

        cookies= new Cookie(ProtoDaoConstant.COOKIE_ADDRESS,address);
        request.setCookies(cookies);
        return request;
    }
}