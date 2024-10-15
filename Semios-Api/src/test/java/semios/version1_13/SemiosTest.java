package semios.version1_13;

// 1.12 的需求

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
import semios.api.SemiosApiApplication;
import semios.api.controller.MakerInfoStatisticsController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.vo.req.Maker.AnalyticsTokenParam;
import semios.api.model.vo.res.Maker.AnalyticsBalanceVo;
import semios.api.model.vo.res.Maker.AnalyticsMakerVo;
import semios.api.model.vo.res.Maker.MakerOwnerInfoVo;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class SemiosTest {


    private final String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";

    @Resource
    MakerInfoStatisticsController makerInfoStatisticsController;


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


    // DaoController include
    @Test
    @Order(1)
    void analyticsToken(){

        Result<AnalyticsBalanceVo> returnVo;


        AnalyticsTokenParam analyticsTokenParam = new AnalyticsTokenParam();
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("null param result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // param error
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(1);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("err param 1 result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setType(1);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("err param 2 result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // error param
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(20000);    // dao is not exist
        analyticsTokenParam.setType(2);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("dao is not exist result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.AUTH_ERROR.getResultCode(),returnVo.getResultCode());

        // success
        // input token
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(2);    // seed node
        analyticsTokenParam.setType(1);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("input token result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        // out put token
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(2);
        analyticsTokenParam.setType(2);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("out put token result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(1);    // sub node
        analyticsTokenParam.setType(2);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("sub node 1 result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(10);    // sub node
        analyticsTokenParam.setType(2);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("sub node 2 result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(22);    // sub node
        analyticsTokenParam.setType(2);
        returnVo = makerInfoStatisticsController.analyticsToken(analyticsTokenParam,request);
        log.info("sub node 3 result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    @Order(5)
    void analyticsMaker(){

        Result<AnalyticsMakerVo> returnVo;


        AnalyticsTokenParam analyticsTokenParam = new AnalyticsTokenParam();
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("null param maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // param error

        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setType(1);
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("error param 1 maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        // error param
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(20000);    // dao is not exist
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("dao is not exist maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.AUTH_ERROR.getResultCode(),returnVo.getResultCode());



        // success
        // input token
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(2);    // seed node
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("seed node maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(1);    // sub node
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("sub node 1 maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(10);    // sub node
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("sub node 2 maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(22);    // sub node
        returnVo = makerInfoStatisticsController.analyticsMaker(analyticsTokenParam,request);
        log.info("sub node 3 maker result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    @Order(10)
    void analyticsMakerList(){

        Result<MakerOwnerInfoVo> returnVo;


        AnalyticsTokenParam analyticsTokenParam = new AnalyticsTokenParam();
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("null param analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // param error
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setType(1);
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("error param 1 analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        // error param
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(20000);    // dao is not exist
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("dao is not exist analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.AUTH_ERROR.getResultCode(),returnVo.getResultCode());


        // success
        // input token
        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(2);    // seed node
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("seed node analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(1);    // sub node
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("sub node 1 analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(10);    // sub node
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("sub node 2 analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(22);    // sub node
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("sub node 3 analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        analyticsTokenParam = new AnalyticsTokenParam();
        analyticsTokenParam.setDaoId(11);    // seed node
        returnVo = makerInfoStatisticsController.analyticsMakerList(analyticsTokenParam,request);
        log.info("sub node 4 analyticsMakerList result:{}",JacksonUtil.obj2json(returnVo.getData()));
        Assert.assertEquals("接口结果", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
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