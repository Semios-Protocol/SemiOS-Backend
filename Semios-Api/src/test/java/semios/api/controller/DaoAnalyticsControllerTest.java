package semios.api.controller;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.vo.res.*;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.TreasuryTogetherDaoListVo;
import semios.api.model.vo.req.DaoIdParam;
import semios.api.model.vo.req.DaoIdReqVo;
import semios.api.utils.JacksonUtil;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;


@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class DaoAnalyticsControllerTest {

    @Resource
    DaoAnalyticsController daoAnalyticsController;

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoInfo(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    @Disabled("TODO: Complete this test")
    void testTogetherDaoInfo() {
        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();

        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        daoIdReqVo.setDaoId("42");
        daoAnalyticsController.togetherDaoInfo(daoIdReqVo, new MockHttpServletRequest());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoInfo(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    void testTogetherDaoInfo2() {
        //   Diffblue Cover was unable to write a Spring test,
        //   so wrote a non-Spring test instead.
        //   Reason: R010 Timeout.
        //   Creating the arrange/act section of your test took more than
        //   20 seconds. This often happens because Diffblue Cover ran code in your
        //   project which requests user input (System.in), blocks on a lock, or runs into
        //   an infinite or very long loop.
        //   See https://diff.blue/R010 to resolve this issue.

        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        Result<TogetherDaoDetailVo> actualTogetherDaoInfoResult = daoAnalyticsController.togetherDaoInfo(null,
                new MockHttpServletRequest());
        assertEquals("Parameter error,", actualTogetherDaoInfoResult.getResultDesc());
        assertEquals(201, actualTogetherDaoInfoResult.getResultCode());
        assertTrue(actualTogetherDaoInfoResult.getDataList().isEmpty());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoInfo(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    @Disabled("TODO: Complete this test")
    void testTogetherDaoInfo3() {
        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        DaoIdReqVo daoIdReqVo = mock(DaoIdReqVo.class);
        when(daoIdReqVo.getDaoId()).thenReturn("42");
        doNothing().when(daoIdReqVo).setDaoId((String) any());
        doNothing().when(daoIdReqVo).setType((Integer) any());
        doNothing().when(daoIdReqVo).setUserAddress((String) any());
        daoIdReqVo.setDaoId("42");
        daoIdReqVo.setType(1);
        daoIdReqVo.setUserAddress("42 Main St");
        daoAnalyticsController.togetherDaoInfo(daoIdReqVo, new MockHttpServletRequest());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoInfo(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    @Disabled("TODO: Complete this test")
    void testTogetherDaoInfo4() {
        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        DaoIdReqVo daoIdReqVo = mock(DaoIdReqVo.class);
        when(daoIdReqVo.getDaoId()).thenReturn("foo");
        doNothing().when(daoIdReqVo).setDaoId((String) any());
        doNothing().when(daoIdReqVo).setType((Integer) any());
        doNothing().when(daoIdReqVo).setUserAddress((String) any());
        daoIdReqVo.setDaoId("42");
        daoIdReqVo.setType(1);
        daoIdReqVo.setUserAddress("42 Main St");
        daoAnalyticsController.togetherDaoInfo(daoIdReqVo, new MockHttpServletRequest());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoInfo(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    void testTogetherDaoInfo5() {
        //   Diffblue Cover was unable to write a Spring test,
        //   so wrote a non-Spring test instead.
        //   Reason: R010 Timeout.
        //   Creating the arrange/act section of your test took more than
        //   20 seconds. This often happens because Diffblue Cover ran code in your
        //   project which requests user input (System.in), blocks on a lock, or runs into
        //   an infinite or very long loop.
        //   See https://diff.blue/R010 to resolve this issue.

        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        DaoIdReqVo daoIdReqVo = mock(DaoIdReqVo.class);
        when(daoIdReqVo.getDaoId()).thenReturn("");
        Result<TogetherDaoDetailVo> actualTogetherDaoInfoResult = daoAnalyticsController.togetherDaoInfo(daoIdReqVo,
                new MockHttpServletRequest());
        assertEquals("Parameter error,", actualTogetherDaoInfoResult.getResultDesc());
        assertEquals(201, actualTogetherDaoInfoResult.getResultCode());
        assertTrue(actualTogetherDaoInfoResult.getDataList().isEmpty());
        verify(daoIdReqVo).getDaoId();
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoToken(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    @Disabled("TODO: Complete this test")
    void testTogetherDaoToken() {
        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();

        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        daoIdReqVo.setDaoId("42");
        daoIdReqVo.setType(1);
        daoIdReqVo.setUserAddress("42 Main St");
        daoAnalyticsController.togetherDaoToken(daoIdReqVo, new MockHttpServletRequest());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoToken(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    void testTogetherDaoToken2() {
        //   Diffblue Cover was unable to write a Spring test,
        //   so wrote a non-Spring test instead.
        //   Reason: R010 Timeout.
        //   Creating the arrange/act section of your test took more than
        //   20 seconds. This often happens because Diffblue Cover ran code in your
        //   project which requests user input (System.in), blocks on a lock, or runs into
        //   an infinite or very long loop.
        //   See https://diff.blue/R010 to resolve this issue.

        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        Result<TogetherDaoTokenVo> actualTogetherDaoTokenResult = daoAnalyticsController.togetherDaoToken(null,
                new MockHttpServletRequest());
        assertEquals("Parameter error,", actualTogetherDaoTokenResult.getResultDesc());
        assertEquals(201, actualTogetherDaoTokenResult.getResultCode());
        assertTrue(actualTogetherDaoTokenResult.getDataList().isEmpty());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoToken(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    @Disabled("TODO: Complete this test")
    void testTogetherDaoToken3() {
        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        DaoIdReqVo daoIdReqVo = mock(DaoIdReqVo.class);
        when(daoIdReqVo.getDaoId()).thenReturn("42");
        doNothing().when(daoIdReqVo).setDaoId((String) any());
        doNothing().when(daoIdReqVo).setType((Integer) any());
        doNothing().when(daoIdReqVo).setUserAddress((String) any());
        daoIdReqVo.setDaoId("42");
        daoIdReqVo.setType(1);
        daoIdReqVo.setUserAddress("42 Main St");
        daoAnalyticsController.togetherDaoToken(daoIdReqVo, new MockHttpServletRequest());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoToken(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    @Disabled("TODO: Complete this test")
    void testTogetherDaoToken4() {
        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        DaoIdReqVo daoIdReqVo = mock(DaoIdReqVo.class);
        when(daoIdReqVo.getDaoId()).thenReturn("foo");
        doNothing().when(daoIdReqVo).setDaoId((String) any());
        doNothing().when(daoIdReqVo).setType((Integer) any());
        doNothing().when(daoIdReqVo).setUserAddress((String) any());
        daoIdReqVo.setDaoId("42");
        daoIdReqVo.setType(1);
        daoIdReqVo.setUserAddress("42 Main St");
        daoAnalyticsController.togetherDaoToken(daoIdReqVo, new MockHttpServletRequest());
    }

    /**
     * Method under test: {@link DaoAnalyticsController#togetherDaoToken(DaoIdReqVo, HttpServletRequest)}
     */
    @Test
    void testTogetherDaoToken5() {
        //   Diffblue Cover was unable to write a Spring test,
        //   so wrote a non-Spring test instead.
        //   Reason: R010 Timeout.
        //   Creating the arrange/act section of your test took more than
        //   20 seconds. This often happens because Diffblue Cover ran code in your
        //   project which requests user input (System.in), blocks on a lock, or runs into
        //   an infinite or very long loop.
        //   See https://diff.blue/R010 to resolve this issue.

        DaoAnalyticsController daoAnalyticsController = new DaoAnalyticsController();
        DaoIdReqVo daoIdReqVo = mock(DaoIdReqVo.class);
        when(daoIdReqVo.getDaoId()).thenReturn("");
        Result<TogetherDaoTokenVo> actualTogetherDaoTokenResult = daoAnalyticsController.togetherDaoToken(daoIdReqVo,
                new MockHttpServletRequest());
        assertEquals("Parameter error,", actualTogetherDaoTokenResult.getResultDesc());
        assertEquals(201, actualTogetherDaoTokenResult.getResultCode());
        assertTrue(actualTogetherDaoTokenResult.getDataList().isEmpty());
        verify(daoIdReqVo).getDaoId();
    }


    @Test
    void topupBalanceMore() {
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        Result<TogetherDaoMakerVo> returnVo;

        // 传入 空的 dao id
        daoIdReqVo.setDaoId(null);
        returnVo = daoAnalyticsController.togetherDaoMaker(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdReqVo.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherDaoMaker(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdReqVo.setDaoId("165");
        returnVo = daoAnalyticsController.togetherDaoMaker(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }


    @Test
    void togetherDaoToken() {
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        Result<TogetherDaoTokenVo> returnVo;

        // 传入 空的 dao id
        daoIdReqVo.setDaoId(null);
        returnVo = daoAnalyticsController.togetherDaoToken(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdReqVo.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherDaoToken(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdReqVo.setDaoId("165");
        returnVo = daoAnalyticsController.togetherDaoToken(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }

    @Test
    void togetherDaoMakerList() {
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        Result<TogetherDaoMakerListVo> returnVo;

        // 传入 空的 dao id
        daoIdReqVo.setDaoId(null);
        returnVo = daoAnalyticsController.togetherDaoMakerList(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdReqVo.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherDaoMakerList(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdReqVo.setDaoId("165");
        returnVo = daoAnalyticsController.togetherDaoMakerList(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }

    @Test
    void togetherDaoTreasury() {
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        Result<TogetherDaoTreasuryVo> returnVo;

        // 传入 空的 dao id
        daoIdReqVo.setDaoId(null);
        returnVo = daoAnalyticsController.togetherDaoTreasury(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdReqVo.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherDaoTreasury(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdReqVo.setDaoId("165");
        returnVo = daoAnalyticsController.togetherDaoTreasury(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }

    @Test
    void togetherDaoTreasuryInfo() {
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        Result<TogetherDaoTreasuryInfoVo> returnVo;

        // 传入 空的 dao id
        daoIdReqVo.setDaoId(null);
        returnVo = daoAnalyticsController.togetherDaoTreasuryInfo(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdReqVo.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherDaoTreasuryInfo(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdReqVo.setDaoId("165");
        returnVo = daoAnalyticsController.togetherDaoTreasuryInfo(daoIdReqVo,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }


    @Test
    void togetherDaoTreasuryTransaction() {
        DaoIdParam daoIdParam = new DaoIdParam();
        ResultList<TogetherDaoTreasuryTransactionVo> returnVo;

        // 传入 空的 dao id
        daoIdParam.setDaoId(null);
        returnVo = daoAnalyticsController.togetherDaoTreasuryTransaction(daoIdParam,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdParam.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherDaoTreasuryTransaction(daoIdParam,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdParam.setDaoId("165");
        returnVo = daoAnalyticsController.togetherDaoTreasuryTransaction(daoIdParam,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }

    @Test
    void togetherTreasuryDaoList() {
        DaoIdParam daoIdParam = new DaoIdParam();
        ResultList<TreasuryTogetherDaoListVo> returnVo;

        // 传入 空的 dao id
        daoIdParam.setDaoId(null);
        returnVo = daoAnalyticsController.togetherTreasuryDaoList(daoIdParam,new MockHttpServletRequest());
        assertEquals(ResultDesc.PARAM_ERROR.getResultCode(), returnVo.getResultCode(), "成功");

        // 传入 不存在 的dao id
        daoIdParam.setDaoId("112345678");
        returnVo = daoAnalyticsController.togetherTreasuryDaoList(daoIdParam,new MockHttpServletRequest());
        assertEquals(ResultDesc.NOT_FOUND_ERROR.getResultCode(),returnVo.getResultCode());
        assertTrue(returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传入正确的 dao id--需要是 聚合 dao id
        daoIdParam.setDaoId("165");
        returnVo = daoAnalyticsController.togetherTreasuryDaoList(daoIdParam,new MockHttpServletRequest());
        assertEquals(ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        System.out.println("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
    }


}

