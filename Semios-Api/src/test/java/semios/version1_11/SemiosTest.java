package semios.version1_11;

// 1.11 的需求

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
import semios.api.controller.DaoController;
import semios.api.controller.NodePermissionNftController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.vo.req.DaoExportInfoParam.DaoExportParam;
import semios.api.model.vo.res.DaoExportInfo.DaoExportInfoVo;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import java.util.ArrayList;
import java.util.List;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class SemiosTest {


    // 模仿session
    private static final MockHttpSession session = new MockHttpSession();
    // 模仿request
    private static MockHttpServletRequest request = new MockHttpServletRequest();
    // 模仿response
    private static final MockHttpServletResponse response = new MockHttpServletResponse();
    private final String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";
    @Resource
    NodePermissionNftController nodePermissionNftController;
    @Resource
    DaoController daoController;

    @Test
    @Order(0)
    void startApplication() {
        SpringBeanUtil.applicationContext = SpringApplication.run(SemiosApiApplication.class);
    }


    // NodePermissionNftController include
    @Test
    @Order(1)
    void daoExportInfo() {
        List<Result<DaoExportInfoVo>> resultList = new ArrayList<>();

        Result<DaoExportInfoVo> returnVo;
        DaoExportParam daoIdReqVo = new DaoExportParam();
        daoIdReqVo.setUserAddress("0xf8BAf7268F3daeFE4135F7711473aE8b6c3b47d8");
        daoIdReqVo.setType(1);

        daoIdReqVo.setDaoId("1");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("3");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("4");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("5");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("6");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("8");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("10");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("12");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        daoIdReqVo.setDaoId("14");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());


        daoIdReqVo.setDaoId("16");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());


        daoIdReqVo.setDaoId("18");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());


        daoIdReqVo.setDaoId("19");
        returnVo = daoController.daoExportInfo(daoIdReqVo, addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        resultList.add(returnVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        log.info(JacksonUtil.obj2json(resultList));
    }


    private MockHttpServletRequest addressRequest(String address) {
        Cookie cookies;
        request = new MockHttpServletRequest();
        request.setCharacterEncoding("UTF-8");

        cookies = new Cookie(ProtoDaoConstant.COOKIE_ADDRESS, address);
        request.setCookies(cookies);
        return request;
    }
}