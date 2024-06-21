package semios.api.controller;

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
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.vo.res.*;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.DaoIdReqVo;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.req.UserProfileReqVo;
import semios.api.model.vo.req.UserSignatureReqVo;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpSession;

import java.io.File;
import java.io.FileInputStream;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class UserControllerTest {

    @Resource
    UserController userController;

    // 模仿session
    private static MockHttpSession session = new MockHttpSession();
    // 模仿request
    private static MockHttpServletRequest request = new MockHttpServletRequest();
    private static MockHttpServletResponse response = new MockHttpServletResponse();

    @Test
    @Order(0)
    void  startApplication(){
        SpringBeanUtil.applicationContext = SpringApplication.run(SemiosApiApplication.class);
    }

    @Test
    @Order(1)
    void userLogin(){
        startApplication();

        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        Result<Boolean> returnVo;

        returnVo = userController.userLogin(userProfileReqVo,request,response);
        Assert.assertEquals("成功", ResultDesc.FAIL.getResultCode(),returnVo.getResultCode());

        userProfileReqVo.setUserAddress("0x1234567890abcdef1234567890abcdef12345678");  // user is not existed
        returnVo = userController.userLogin(userProfileReqVo,request,response);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertFalse("成功",returnVo.getData());

        userProfileReqVo.setUserAddress("0x1234567890abcdef1234567890abcdef12345680");  // user is existed,but not sign
        returnVo = userController.userLogin(userProfileReqVo,request,response);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertFalse("成功",returnVo.getData());

        userProfileReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526bef");  // user is existed, already sign
        returnVo = userController.userLogin(userProfileReqVo,request,response);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertTrue("成功",returnVo.getData());

        // user login success and get cookie
        Cookie[] cookies = response.getCookies();
        Assert.assertTrue("成功",cookies.length>0);

        // user login success and get session
        HttpSession session = request.getSession();
        assert session!=null;
        String address = (String) session.getAttribute("user");
        Assert.assertEquals("成功", "0xc537a223b7fe86483d31442248c5918177526bef",address);
    }

    @Test
    @Order(5)
    void userProfileInfo(){
        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        Result<UserProfileResVo> returnVo;

        userProfileReqVo.setUserAddress("0x1234567890abcdef1234567890abcdef12345678");
        returnVo = userController.userProfileInfo(userProfileReqVo);
        Assert.assertEquals("成功", ResultDesc.ERROR.getResultCode(),returnVo.getResultCode());

        userProfileReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526bef");
        returnVo = userController.userProfileInfo(userProfileReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertNotNull("成功", returnVo.getData());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getData()));
    }

    @Test
    @Order(8)
    void userProfileSave() throws Exception {
        UserProfileVo userProfileVo = new UserProfileVo();
        Result<Boolean> returnVo;
        userProfileVo.setIntroduction("this is introduction");


        returnVo = userController.userProfileSave(userProfileVo,request,null,response);
        Assert.assertEquals("成功", ResultDesc.USER_ERROR.getResultCode(),returnVo.getResultCode());  // not logged in
        Assert.assertEquals("成功", "please login.",returnVo.getResultDesc());


        userProfileVo.setUserName("test-user"); // name is illegal
        request = addressRequest("0xc537a223b7fe86483d31442248c5918177526bef");  // mock login
        returnVo = userController.userProfileSave(userProfileVo,request,null,response);
        Assert.assertEquals("成功", ResultDesc.ERROR.getResultCode(),returnVo.getResultCode());


        userProfileVo.setUserName("test");  // name is already taken
        returnVo = userController.userProfileSave(userProfileVo,request,null,response);
        Assert.assertEquals("成功", ResultDesc.ERROR.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", "Invalid name . The name is already taken.",returnVo.getResultDesc());


        request = addressRequest("0xc537a223b7fe86483d31442248c5918177526cde");  // user is not existed
        userProfileVo.setUserName("test test");
        returnVo = userController.userProfileSave(userProfileVo,request,null,response);
        Assert.assertEquals("成功", ResultDesc.ERROR.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", "用户不存在！",returnVo.getResultDesc());


        request = addressRequest("0xc537a223b7fe86483d31442248c5918177526bef");  // user is  existed
        returnVo = userController.userProfileSave(userProfileVo,request,null,response);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        // add avatar file
        try {
            File file = new File("src/main/resources/image/work_default.png");
            FileInputStream input = new FileInputStream(file);
            MultipartFile multipartFile = new MockMultipartFile("file",
                    file.getName(), "image/jpeg", input);
            returnVo = userController.userProfileSave(userProfileVo,request,multipartFile,response);
            Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        }catch (Exception e){
            throw new Exception(e);
        }

    }


    @Test
    @Order(10)
    void userIncomeForDao() {
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        Result<UserDaoIncomeVo> returnVo;

        // have dao value
        userProfilePageReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526bef");
        returnVo = userController.userIncomeForDao(userProfilePageReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()!=null && !returnVo.getDataList().isEmpty());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getDataList()));

        // user is not existed
        userProfilePageReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526cde");
        returnVo = userController.userIncomeForDao(userProfilePageReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()!=null && returnVo.getDataList().isEmpty());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getDataList()));
    }

    @Test
    @Order(15)
    void userIncomeForCanvas() {
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        Result<UserCanvasIncomeVo> returnVo;

        // have dao value
        userProfilePageReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526bef");
        returnVo = userController.userIncomeForCanvas(userProfilePageReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()!=null && !returnVo.getDataList().isEmpty());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getDataList()));

        // user is not existed
        userProfilePageReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526cde");
        returnVo = userController.userIncomeForCanvas(userProfilePageReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()!=null && returnVo.getDataList().isEmpty());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getDataList()));
    }


    @Test
    @Order(18)
    void signature() {
        Result<String> returnVo;

        String signatureHash = "0x535329df013d1a43df095a590a372581b4be8dcf3ef7dd97fafe74453d3f2af6516aabb19a6f8426dd163d6abbbd12c5ea1b126a53a57d93a427d1ab8bb3c7da1b";
        String originalText = "Welcome to Semios!\\n\\nThis request will not trigger a blockchain transaction or cost any gas fees.\\n\\nYour authentication status will reset after 24 hours.\\n\\nWallet address:\\n0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266\\n\\nNonce:\\np8af984e-0jux-qjzq-nkm0-a32mawpy5h8f";

        UserSignatureReqVo userSignatureReqVo = new UserSignatureReqVo();
        userSignatureReqVo.setUserAddress("0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266");
        userSignatureReqVo.setSignatureHash(signatureHash);
        userSignatureReqVo.setOriginalText(originalText);

        request = addressRequest("0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266"); // user is not existed
        returnVo = userController.signature(userSignatureReqVo,request,response);
        // need data
        Assert.assertEquals("成功", ResultDesc.ERROR.getResultCode(),returnVo.getResultCode());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getData()));
    }




    @Test
    void topupBalance() {
        UserProfileReqVo userProfileReqVo = new UserProfileReqVo();
        ResultList<UserTopupBalanceVo> returnVo;
        // 用户未登陆情况
        returnVo = userController.topupBalance(userProfileReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()==null || returnVo.getDataList().isEmpty());

        userProfileReqVo.setUserAddress("0xc537a223b7fe86483d31442248c5918177526bef");
        returnVo = userController.topupBalance(userProfileReqVo);
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()!=null && !returnVo.getDataList().isEmpty());
        log.info("返回的结果为:"+ JacksonUtil.obj2json(returnVo.getDataList()));
    }

    @Test
    void topupBalanceMore() {
        DaoIdReqVo daoIdReqVo = new DaoIdReqVo();
        ResultList<UserTopupBalanceDetailsVo> returnVo;

        // 用户未登陆情况
        daoIdReqVo.setDaoId("123");
        returnVo = userController.topupBalanceMore(daoIdReqVo,request);
        Assert.assertEquals("成功", ResultDesc.USER_ERROR.getResultCode(),returnVo.getResultCode());

        // 用户登陆  -- 传入 空的dao id
        daoIdReqVo.setDaoId(null);
        returnVo = userController.topupBalanceMore(daoIdReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        //  用户登陆  -- 传入 不存在 的dao id
        daoIdReqVo.setDaoId("1q2w3e4r");
        returnVo = userController.topupBalanceMore(daoIdReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getDataList()==null || returnVo.getDataList().isEmpty());

        // 用户登陆 -- 传入正确的 dao id
        daoIdReqVo.setDaoId("165");
        returnVo = userController.topupBalanceMore(daoIdReqVo,addressRequest("0xc537a223b7fe86483d31442248c5918177526bef"));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        log.info("返回的值为:"+JacksonUtil.obj2json(returnVo.getDataList()));
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