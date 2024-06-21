package semios.version1_8;

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
import semios.api.controller.IncentivePlanController;
import semios.api.controller.UserController;
import semios.api.model.vo.req.DaoIdParam;
import semios.api.model.vo.req.DaoProjectVo;
import semios.api.model.vo.req.UserProfileReqVo;
import semios.api.model.vo.res.UserTopupBalancePendingVo;
import semios.api.model.vo.res.UserTopupBalanceVo;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.Plan.CreatePlanParam;
import semios.api.model.vo.req.Plan.PlanIdReqVo;
import semios.api.model.vo.res.Plan.CreatePlanParamVo;
import semios.api.model.vo.res.Plan.PlanBasicInfoVo;
import semios.api.model.vo.res.Plan.PlanListVo;
import semios.api.model.vo.res.Plan.TogetherPlanVo;
import semios.api.model.vo.res.TopUpReward.TopupNftListVo;
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
    IncentivePlanController incentivePlanController;

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


    // IncentivePlanController include
    @Test
    @Order(1)
    void createPlanInfo(){
        CreatePlanParam createPlanParam = new CreatePlanParam();
        createPlanParam.setDuration(3);
        createPlanParam.setPlanStartDate("2024-05-12");
        createPlanParam.setUserAddress(address);
        createPlanParam.setProjectId("eaf623688818836f3d4bcde3e5fa3b875a587b48bf16d9a3cef0e95b5342fced");
        Result<CreatePlanParamVo> returnVo;

        returnVo =  incentivePlanController.createPlanInfo(createPlanParam,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertTrue("成功", returnVo.getDataList() != null || !returnVo.getDataList().isEmpty());
    }

    @Test
    @Order(2)
    void togetherDaoPlan(){
        DaoIdParam daoIdParam = new DaoIdParam();
        daoIdParam.setDaoId("267");

        Result<TogetherPlanVo> returnVo;

        returnVo = incentivePlanController.togetherDaoPlan(daoIdParam,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        daoIdParam.setDaoId("266");
        returnVo = incentivePlanController.togetherDaoPlan(daoIdParam,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.AUTH_ERROR.getResultCode(),returnVo.getResultCode());
    }


    @Test
    @Order(3)
    void togetherPlanList(){
        DaoIdParam daoIdParam = new DaoIdParam();
        daoIdParam.setDaoId("267");

        ResultList<PlanListVo> returnVo;
        returnVo = incentivePlanController.togetherPlanList(daoIdParam,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    @Order(4)
    void planBaseInfo(){
        PlanIdReqVo planIdReqVo = new PlanIdReqVo();
        planIdReqVo.setPlanId(9999);

        Result<PlanBasicInfoVo> returnVo;
        returnVo = incentivePlanController.planBaseInfo(planIdReqVo,request);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.AUTH_ERROR.getResultCode(),returnVo.getResultCode());

        // 添加测试数据
    }

    // UserController include
    @Test
    @Order(20)
    void topupBalance(){
        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        userProfileReqVo.setUserAddress(address);

        ResultList<UserTopupBalanceVo> returnVo;

        returnVo = userController.topupBalance(userProfileReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    @Order(21)
    void topupBalancePending(){
        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        userProfileReqVo.setUserAddress(address);

        ResultList<UserTopupBalancePendingVo> returnVo;

        returnVo = userController.topupBalancePending(userProfileReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    @Order(22)
    void topupBalanceReward(){
        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        userProfileReqVo.setUserAddress(address);

        ResultList<UserTopupRewardVo> returnVo;

        returnVo = userController.topupBalanceReward(userProfileReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    @Order(23)
    void topupNftList(){
        DaoProjectVo daoProjectVo = new DaoProjectVo();
        ResultList<TopupNftListVo> returnVo;

        returnVo = userController.topupNftList(daoProjectVo);
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