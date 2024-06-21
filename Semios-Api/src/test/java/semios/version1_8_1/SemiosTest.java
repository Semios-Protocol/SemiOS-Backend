package semios.version1_8_1;

// 1.8的需求

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
import semios.api.controller.IncentivePlanController;
import semios.api.controller.UserController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.DaoProjectVo;
import semios.api.model.vo.req.UserProfileReqVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardDetailVo;
import semios.api.model.vo.res.TopUpReward.UserTopupRewardVo;
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
    IncentivePlanController  incentivePlanController;

    @Resource
    UserController userController;


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



    // UserController include
    @Test
    @Order(22)
    void topupBalanceReward(){
        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        userProfileReqVo.setUserAddress("0xf8BAf7268F3daeFE4135F7711473aE8b6c3b47d8");

        ResultList<UserTopupRewardVo> returnVo;

        returnVo = userController.topupBalanceReward(userProfileReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    @Order(23)
    void topupBalanceRewardDetail(){
        DaoProjectVo daoProjectVo = new DaoProjectVo();
        daoProjectVo.setUserAddress("0xf8BAf7268F3daeFE4135F7711473aE8b6c3b47d8");
        daoProjectVo.setProjectId("f2022f365c8874f3cf274cbedeb64ead8762bdeb1d22b9345e6e4a0324b40942");

        ResultList<UserTopupRewardDetailVo> returnVo;

        returnVo = userController.topupBalanceRewardDetail(daoProjectVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
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