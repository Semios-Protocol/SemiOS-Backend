package semios.api.controller;

import lombok.extern.slf4j.Slf4j;
import org.junit.Assert;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.vo.req.*;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.res.BaseWorkVo.WorkNftDetailsVo;
import semios.api.model.vo.res.MineNftVo;
import semios.api.model.vo.res.WorkDetailResVo;
import semios.api.model.vo.res.WorkListVo;
import semios.api.model.vo.res.WorkLockDuration;
import semios.api.service.mock.BaseService;
import semios.api.utils.JacksonUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class WorkControllerTest extends BaseService {

    private final String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";

    @Resource
    WorkController workController;

    // 模仿session
    private static MockHttpSession session = new MockHttpSession();
    // 模仿request
    private static MockHttpServletRequest request = new MockHttpServletRequest();
    // 模仿response
    private static MockHttpServletResponse response = new MockHttpServletResponse();

    @Test
    void exploreUnmintedWorks() {
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();
        Result<WorkListVo> returnVo;

        returnVo = workController.exploreUnmintedWorks(daoSortedReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        // select by dao id
        daoSortedReqVo.setDaoId("1"); // dao id
        returnVo = workController.exploreUnmintedWorks(daoSortedReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    void workHole() {
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        Result<WorkListVo> returnVo;
        addressRequest("0xc537a223b7fe86483d31442248c5918177526bef");
        returnVo = workController.workHole(userProfilePageReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    void workMinted() {
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        addressRequest("0xc537a223b7fe86483d31442248c5918177526bef");
        Result<WorkListVo> returnVo;
        returnVo = workController.workMinted(userProfilePageReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    void workMintTopUp(){
        ResultList<MineNftVo> returnVo = new ResultList<>();

        // 不用分页情况，传入-1
        DaoIdParam daoIdParam = new DaoIdParam();
        daoIdParam.setPageSize(-1L);  // 查询全部数据

        // 测试 - 拥有top up balance的账户
        returnVo = workController.workMintTopUp(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),daoIdParam);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true, !returnVo.getDataList().isEmpty());

        // 测试 - 未拥有top up balance的账户
        returnVo = workController.workMintTopUp(addressRequest("0x1abadccca23fc3250dc6f765147475016b881cd1"),daoIdParam);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true, returnVo.getDataList().isEmpty());
    }

    @Test
    void workMintNotTopUp(){
        ResultList<MineNftVo> returnVo;

        // 不用分页情况，传入-1
        DaoIdParam daoIdParam = new DaoIdParam();
        daoIdParam.setPageSize(-1L);  // 查询全部数据


        // 不传值的情况
        returnVo = workController.workMintNotTopUp(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),daoIdParam);
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // 传入不存在的dao的情况
        daoIdParam.setDaoId("1");
        returnVo = workController.workMintNotTopUp(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),daoIdParam);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() == null || returnVo.getDataList().isEmpty());


        // 测试 - dao 下账户没有top up balance
        daoIdParam.setDaoId("203");
        returnVo = workController.workMintNotTopUp(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),daoIdParam);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 测试 - dao 下账户有有top up balance
        daoIdParam.setDaoId("165");
        returnVo = workController.workMintNotTopUp(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),daoIdParam);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() != null && !returnVo.getDataList().isEmpty());
    }


    @Test
    void workDetailNft(){
        WorkId workReqVo = new WorkId();
        workReqVo.setPageSize(-1L);  // 查询全部数据

        ResultList<WorkNftDetailsVo> returnVo;

        // 不传值的情况
        returnVo = workController.workDetailNft(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),workReqVo);
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));

        // 传的值不存在的情况
        workReqVo.setWorkId("12874682");
        returnVo = workController.workDetailNft(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),workReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传的值正确
        workReqVo.setWorkId("402");
        returnVo = workController.workDetailNft(addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"),workReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() != null && !returnVo.getDataList().isEmpty());

    }

    @Test
    void workLockDuration(){
        WorkLockHours workLockHours = new WorkLockHours();

        Result<WorkLockDuration> returnVo;

        // 不传值的情况
        returnVo = workController.workLockDuration(workLockHours);
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        // 传入 违法 极大值的情况
        workLockHours.setHours(1234567890);
        returnVo = workController.workLockDuration(workLockHours);
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // 传入 违法 极小值的情况
        workLockHours.setHours(-1234567890);
        returnVo = workController.workLockDuration(workLockHours);
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        // 传入 违法 边缘值
        workLockHours.setHours(0);
        returnVo = workController.workLockDuration(workLockHours);
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        // 传入 违法  非整数
//        workLockHours.setHours(new Integer("1.1"));
//        returnVo = workController.workLockDuration(workLockHours);
//        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        // 传入 合法(调用时需要注释掉 feign逻辑)--feign.RetryableException: Connection refused (Connection refused) executing POST http://127.0.0.1:9381/event/eth/blockNumber?netWork=sepolia
        // 调用内部服务失败，待调研
        workLockHours.setHours(1);
        returnVo = workController.workLockDuration(workLockHours);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info("返回的值为:{}",JacksonUtil.obj2json(returnVo.getData()));
    }

    @Test
    void workDetail(){
        WorkReqVo workReqVo= new WorkReqVo();

        Result<WorkDetailResVo> returnVo;

        // 不传值的情况
        returnVo = workController.workDetail(workReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());
        log.info(JacksonUtil.obj2json(returnVo));

        // 传的值不存在的情况
        workReqVo.setWorkId("12874682");
        returnVo = workController.workDetail(workReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() == null || returnVo.getDataList().isEmpty());

        // 传的值正确
        workReqVo.setWorkId("402");
        returnVo = workController.workDetail(workReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", true ,returnVo.getDataList() != null && !returnVo.getDataList().isEmpty());

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