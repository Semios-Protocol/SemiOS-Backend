package semios.version1_10;

// 1.10 的需求

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
import semios.api.controller.NodePermissionNftController;
import semios.api.controller.UserController;
import semios.api.controller.WorkController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.enums.NodePermissionTypeEnum;
import semios.api.model.vo.req.DaoTransactionHashReqVo;
import semios.api.model.vo.req.NodePermission.NodePermission;
import semios.api.model.vo.req.NodePermission.SelectNftPermission;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.res.NodePermission.*;
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
    NodePermissionNftController nodePermissionNftController;

    @Resource
    UserController userController;

    @Resource
    WorkController workController;


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


    // NodePermissionNftController include
    @Test
    @Order(10)
    void nodePermissionNft(){
        NodePermission nodePermission = new NodePermission();
        nodePermission.setUserAddress("0xf8BAf7268F3daeFE4135F7711473aE8b6c3b47d8");

        Result<NodePermissionInfo> returnVo;
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        nodePermission.setDaoId(483);

        nodePermission.setPermissionType(NodePermissionTypeEnum.Edit_SubNode.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());

        nodePermission.setPermissionType(NodePermissionTypeEnum.Edit_OnChain.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());

        nodePermission.setPermissionType(NodePermissionTypeEnum.Edit_Strategies.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());

        nodePermission.setPermissionType(NodePermissionTypeEnum.Proceeds_Ratio.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());

        nodePermission.setPermissionType(NodePermissionTypeEnum.Treasury_Allocation.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", false,returnVo.getData().getIsPermission());

        nodePermission.setDaoId(484);
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());

        nodePermission.setPermissionType(NodePermissionTypeEnum.Edit_SeedNode.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());

        nodePermission.setPermissionType(NodePermissionTypeEnum.Topup_Ratio.getType());
        returnVo = nodePermissionNftController.nodePermissionNft(nodePermission,addressRequest(address));
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", true,returnVo.getData().getIsPermission());
    }


    @Test
    @Order(15)
    void nftPermissionList(){
        SelectNftPermission selectNftPermission = new SelectNftPermission();
        ResultList<NftDetailPermission> returnVo;


        returnVo = nodePermissionNftController.nftPermissionList(selectNftPermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        selectNftPermission.setWorkId(620);
        returnVo = nodePermissionNftController.nftPermissionList(selectNftPermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        selectNftPermission.setWorkId(620);
        selectNftPermission.setPageNo(1L);
        selectNftPermission.setPageSize(5L);
        returnVo = nodePermissionNftController.nftPermissionList(selectNftPermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    @Order(20)
    void nodePermissionList(){
        NodePermission nodePermission = new NodePermission();
        ResultList<NodeDetailPermission> returnVo;

        returnVo = nodePermissionNftController.nodePermissionList(nodePermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());


        nodePermission.setDaoId(534);
        returnVo = nodePermissionNftController.nodePermissionList(nodePermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", 4,returnVo.getDataList().size());


        nodePermission.setDaoId(535);
        returnVo = nodePermissionNftController.nodePermissionList(nodePermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", 3,returnVo.getDataList().size());
    }



    // UserController include
    @Test
    @Order(30)
    void myPermissions(){
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        userProfilePageReqVo.setUserAddress("0xc26829df47f4b7f6c2b6a4b9f5beb581413d23e1");
        userProfilePageReqVo.setPageSize(10L);
        userProfilePageReqVo.setPageNo(1L);

        ResultList<UserPermissionNft> returnVo;

        returnVo = userController.myPermissions(userProfilePageReqVo);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    @Order(35)
    void permissionNftList(){
        SelectNftPermission selectNftPermission = new SelectNftPermission();
        ResultList<SelectPermissionNft> returnVo;

        returnVo = userController.permissionNftList(selectNftPermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.PARAM_ERROR.getResultCode(),returnVo.getResultCode());

        selectNftPermission.setUserAddress("0xc26829df47f4b7f6c2b6a4b9f5beb581413d23e1");
        selectNftPermission.setWorkId(620);
        selectNftPermission.setPageSize(-1L);
        selectNftPermission.setPageNo(-1L);
        returnVo = userController.permissionNftList(selectNftPermission);
        log.info(JacksonUtil.obj2json(returnVo));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }


    @Test
    @Order(40)
    void transactionHashForWork(){
        DaoTransactionHashReqVo daoTransactionHashReqVo = new DaoTransactionHashReqVo();
        daoTransactionHashReqVo.setTransactionHash("0x8ff897e70ff089c034bfe323054f90d876dc6eb7f0819002d8d04600da860780");

        Result<CreateNodeId> returnVo;

        returnVo = workController.transactionHashForWork(request,daoTransactionHashReqVo);
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