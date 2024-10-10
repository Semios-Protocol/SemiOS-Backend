package semios.version1_12;

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
import semios.api.controller.DaoCollectionController;
import semios.api.controller.DaoController;
import semios.api.controller.WorkCollectionController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.DaoInfo.DaoInfoVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.ExploreFilter.NodeModes;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.req.WorkInfo.WorkInfo;
import semios.api.model.vo.res.ExploreFilter.TokenType;
import semios.api.model.vo.res.SeedNodesListVo;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
@Slf4j
class SemiosTest {


    private final String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";

    @Resource
    DaoController daoController;

    @Resource
    WorkCollectionController workCollectionController;

    @Resource
    DaoCollectionController daoCollectionController;


    // 模仿session
    private static MockHttpSession session = new MockHttpSession();
    // 模仿request
    private static MockHttpServletRequest request = new MockHttpServletRequest();
    // 模仿response
    private static MockHttpServletResponse response = new MockHttpServletResponse();

    @Test
    @Order(0)
    void startApplication() {
        SpringBeanUtil.applicationContext = SpringApplication.run(SemiosApiApplication.class);
    }


    // DaoController include
    @Test
    @Order(1)
    void tokenType() {

        Result<TokenType> returnVo;
        SearchReqVo searchReqVo = new SearchReqVo();

        returnVo = daoController.tokenType(searchReqVo);
        log.info("{}", returnVo.getData());
        Assert.assertEquals("接口结果成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 3, returnVo.getData().getTokenTypeList().size());
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDT"));
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDC"));
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("ETH"));


        searchReqVo.setSearchWord("eth");
        returnVo = daoController.tokenType(searchReqVo);
        log.info("{}", returnVo.getData());
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 1, returnVo.getData().getTokenTypeList().size());
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("ETH"));

        searchReqVo.setSearchWord("usdc");
        returnVo = daoController.tokenType(searchReqVo);
        log.info("{}", returnVo.getData());
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 1, returnVo.getData().getTokenTypeList().size());
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDC"));

        searchReqVo.setSearchWord("us");
        returnVo = daoController.tokenType(searchReqVo);
        log.info("{}", returnVo.getData());
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 2, returnVo.getData().getTokenTypeList().size());
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDC"));
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDT"));

        searchReqVo.setSearchWord("0x");
        returnVo = daoController.tokenType(searchReqVo);
        log.info("{}", returnVo.getData());
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 2, returnVo.getData().getTokenTypeList().size());
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDC"));
        Assert.assertTrue("接口数据", returnVo.getData().getTokenTypeList().contains("USDT"));
    }


    // WorkCollectionController include
    @Test
    @Order(10)
    void exploreWorks() {
        Result<WorkInfo> returnVo;
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        // 无过滤条件
        returnVo = workCollectionController.exploreWorks(daoSortedReqVo, addressRequest(address));
        log.info("exploreWorks return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertFalse("接口数据", returnVo.getDataList().isEmpty());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(1)));

        // 过滤开启了top up的
        DaoSortedReqVo daoSortedReqVoTopup = new DaoSortedReqVo();
        daoSortedReqVoTopup.setTopUpMode(true);
        returnVo = workCollectionController.exploreWorks(daoSortedReqVoTopup, addressRequest(address));
        log.info("exploreWorks TOP UP return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(1)));
        List<Integer> topupNodeNodeId = returnVo.getDataList().stream().map(WorkInfo::getDaoId).distinct().collect(Collectors.toList());
        Assert.assertEquals("接口数据量", 3, topupNodeNodeId.size()); // 1,4,10

        // 过滤指定类型的work
        DaoSortedReqVo daoSortedReqVoTokenType = new DaoSortedReqVo();
        List<String> inputTokenTypes = new ArrayList<>();
        inputTokenTypes.add("USDC");
        daoSortedReqVoTokenType.setInputTokenTypes(inputTokenTypes);
        returnVo = workCollectionController.exploreWorks(daoSortedReqVoTokenType, addressRequest(address));
        log.info("exploreWorks USDC return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(1)));
        Assert.assertEquals("接口数据量", 7, returnVo.getDataList().size()); // 1,3,4,5,19

        List<Integer> typeDaoId = returnVo.getDataList().stream().map(WorkInfo::getDaoId).distinct().collect(Collectors.toList());
        Assert.assertEquals("接口数据量", 5, typeDaoId.size()); // 1,3,4,5,19

        // 过滤指定价格类型的
        DaoSortedReqVo daoSortedReqVoPriceType = new DaoSortedReqVo();
        List<Integer> priceType = new ArrayList<>();
        priceType.add(0);
        priceType.add(1);
        daoSortedReqVoPriceType.setPriceType(priceType);
        returnVo = workCollectionController.exploreWorks(daoSortedReqVoPriceType, addressRequest(address));
        log.info("exploreWorks floating price return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(1)));
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getPriceType().equals(2)));


        // 过滤全部条件的
        DaoSortedReqVo daoSortedReqVoAll = new DaoSortedReqVo();
        daoSortedReqVoAll.setTopUpMode(true);
        daoSortedReqVoAll.setInputTokenTypes(inputTokenTypes);
        daoSortedReqVoAll.setPriceType(priceType);
        returnVo = workCollectionController.exploreWorks(daoSortedReqVoAll, addressRequest(address));
        log.info("exploreWorks all filter return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(1)));
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getPriceType().equals(2)));
    }

    @Test
    @Order(15)
    void exploreNft() {
        Result<WorkInfo> returnVo;
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        // 无过滤条件
        returnVo = workCollectionController.exploreNft(daoSortedReqVo, addressRequest(address));
        log.info("exploreNft return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(0)));


        // 过滤开启了top up的
        DaoSortedReqVo daoSortedReqVoTopup = new DaoSortedReqVo();
        daoSortedReqVoTopup.setTopUpMode(true);
        returnVo = workCollectionController.exploreNft(daoSortedReqVoTopup, addressRequest(address));
        log.info("exploreNft TOP UP return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        List<Integer> topupNodeNodeId = returnVo.getDataList().stream().map(WorkInfo::getDaoId).distinct().collect(Collectors.toList());
        Assert.assertEquals("接口数据量", 3, topupNodeNodeId.size()); // 1,4,10

        // 绑定过权限的nft PermissionNft
        DaoSortedReqVo daoSortedReqVoPermissionNft = new DaoSortedReqVo();
        daoSortedReqVoPermissionNft.setIsPermissionNft(true);
        returnVo = workCollectionController.exploreNft(daoSortedReqVoPermissionNft, addressRequest(address));
        log.info("exploreNft PermissionNft return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        int[] permissionNftIds = {2, 4, 6, 8, 9, 15, 17, 19, 20, 21, 22, 24};
        Assert.assertTrue("接口数据", Arrays.stream(permissionNftIds).boxed().collect(Collectors.toList()).containsAll(returnVo.getDataList().stream().map(WorkInfo::getWorkId).collect(Collectors.toList())));

        // 正在锁定的nft
        DaoSortedReqVo daoSortedReqVoLockedNft = new DaoSortedReqVo();
        daoSortedReqVoLockedNft.setLockedNft(true);
        returnVo = workCollectionController.exploreNft(daoSortedReqVoLockedNft, addressRequest(address));
        log.info("exploreNft Locked return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("成功", 1, returnVo.getDataList().size());
        Assert.assertEquals("成功", new Integer(1), returnVo.getDataList().get(0).getWorkId());

        // 过滤指定类型的work
        DaoSortedReqVo daoSortedReqVoTokenType = new DaoSortedReqVo();
        List<String> inputTokenTypes = new ArrayList<>();
        inputTokenTypes.add("USDC");
        daoSortedReqVoTokenType.setInputTokenTypes(inputTokenTypes);
        returnVo = workCollectionController.exploreNft(daoSortedReqVoTokenType, addressRequest(address));
        log.info("exploreNft USDC return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(0)));
        Assert.assertEquals("接口数据量", 7, returnVo.getDataList().size()); // 1,2,4,6,7,8,24
        List<Integer> typeDaoId = returnVo.getDataList().stream().map(WorkInfo::getDaoId).distinct().collect(Collectors.toList());
        Assert.assertEquals("接口数据量", 5, typeDaoId.size()); // 1,3,4,5,19

        // 过滤全部条件的
        DaoSortedReqVo daoSortedReqVoAll = new DaoSortedReqVo();
        daoSortedReqVoAll.setTopUpMode(true);
        daoSortedReqVoAll.setIsPermissionNft(true);
        daoSortedReqVoAll.setLockedNft(true);
        daoSortedReqVoAll.setInputTokenTypes(inputTokenTypes);
        returnVo = workCollectionController.exploreNft(daoSortedReqVoAll, addressRequest(address));
        log.info("exploreNft all filter return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getWorkStatus().equals(0)));
        Assert.assertTrue("接口数据", returnVo.getDataList().stream().noneMatch(work -> work.getTopupMode().equals(false)));
    }


    @Test
    @Order(20)
    void daoCollections() {
        ResultList<DaoInfoVo> returnVo;
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        // 无过滤条件
        returnVo = daoCollectionController.daoCollections(daoSortedReqVo, addressRequest(address));
        log.info("daoCollections return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());

        // 过滤开启了top up的
        DaoSortedReqVo daoSortedReqVoTopup = new DaoSortedReqVo();
        daoSortedReqVoTopup.setTopUpMode(true);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoTopup, addressRequest(address));
        log.info("daoCollections TOP UP return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        List<Integer> topupNodeNodeId = returnVo.getDataList().stream().map(DaoInfoVo::getDaoId).distinct().collect(Collectors.toList());
        Assert.assertEquals("接口数据量", 3, topupNodeNodeId.size()); // 1,4,10

        // 过滤指定类型的dao
        DaoSortedReqVo daoSortedReqVoTokenType = new DaoSortedReqVo();
        List<String> inputTokenTypes = new ArrayList<>();
        inputTokenTypes.add("USDC");
        daoSortedReqVoTokenType.setInputTokenTypes(inputTokenTypes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoTokenType, addressRequest(address));
        log.info("daoCollections USDC return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        List<Integer> typeDaoId = returnVo.getDataList().stream().map(DaoInfoVo::getDaoId).distinct().collect(Collectors.toList());
        Assert.assertEquals("接口数据量", 5, typeDaoId.size()); // 1,3,4,5,19

        DaoSortedReqVo daoSortedReqVoModeStatus = new DaoSortedReqVo();
        NodeModes nodeModes = new NodeModes();

        // 开启了 乐透 模式
        nodeModes.setLotteryMode(true);
        daoSortedReqVoModeStatus.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoModeStatus, addressRequest(address));
        log.info("daoCollections LotteryMode return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 3 - 1, returnVo.getDataList().size()); // 8,19, 21已经停止，不算..

        // 开启了 erc20支付 模式
        nodeModes = new NodeModes();
        nodeModes.setErc20PaymentMode(true);
        daoSortedReqVoModeStatus.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoModeStatus, addressRequest(address));
        log.info("daoCollections PaymentMode return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 1, returnVo.getDataList().size()); // 3
        Assert.assertEquals("成功", new Integer(3), returnVo.getDataList().get(0).getDaoId());

        // 开启了 top up 模式
        nodeModes = new NodeModes();
        nodeModes.setTopupMode(true);
        daoSortedReqVoModeStatus.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoModeStatus, addressRequest(address));
        log.info("daoCollections PaymentMode return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 3, returnVo.getDataList().size()); // 1,4,10

        // 开启了 unifiedPriceMode 模式
        nodeModes = new NodeModes();
        nodeModes.setUnifiedPriceMode(true);
        daoSortedReqVoModeStatus.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoModeStatus, addressRequest(address));
        log.info("daoCollections PaymentMode return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 6 - 1, returnVo.getDataList().size()); // 1,6,8,10,19,21已经停止，不算..


        // 开启了 specialStrategy 模式
        nodeModes = new NodeModes();
        nodeModes.setSpecialStrategy(true);
        daoSortedReqVoModeStatus.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoModeStatus, addressRequest(address));
        log.info("daoCollections PaymentMode return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 2, returnVo.getDataList().size()); // 8,14

        // 开启了 infiniteMode 模式
        nodeModes = new NodeModes();
        nodeModes.setInfiniteMode(true);
        daoSortedReqVoModeStatus.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoModeStatus, addressRequest(address));
        log.info("daoCollections PaymentMode return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 1, returnVo.getDataList().size()); // 5
        Assert.assertEquals("成功", new Integer(5), returnVo.getDataList().get(0).getDaoId());

        // 过滤全部条件的
        DaoSortedReqVo daoSortedReqVoAll = new DaoSortedReqVo();
        daoSortedReqVoAll.setTopUpMode(true);
        daoSortedReqVoAll.setInputTokenTypes(inputTokenTypes);
        nodeModes = new NodeModes();
        nodeModes.setErc20PaymentMode(true);
        nodeModes.setTopupMode(true);
        nodeModes.setUnifiedPriceMode(true);
        nodeModes.setSpecialStrategy(true);
        nodeModes.setInfiniteMode(true);
        daoSortedReqVoAll.setNodeModeStatus(nodeModes);
        returnVo = daoCollectionController.daoCollections(daoSortedReqVoAll, addressRequest(address));
        log.info("daoCollections all filter return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
    }

    @Test
    @Order(20)
    void daoExploreSeedNodes() {
        Result<SeedNodesListVo> returnVo;
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        // 无过滤条件
        returnVo = daoController.daoExploreSeedNodes(daoSortedReqVo, addressRequest(address));
        log.info("daoExploreSeedNodes return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());


        // 过滤开启了top up的
        DaoSortedReqVo daoSortedReqVoTopup = new DaoSortedReqVo();
        daoSortedReqVoTopup.setTopUpMode(true);
        returnVo = daoController.daoExploreSeedNodes(daoSortedReqVoTopup, addressRequest(address));
        log.info("daoExploreSeedNodes TOP UP return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 2, returnVo.getDataList().size()); // 2,11

        // 过滤开启了plan的
        DaoSortedReqVo daoSortedReqVoPlan = new DaoSortedReqVo();
        daoSortedReqVoPlan.setWithIncentivePlan(true);
        returnVo = daoController.daoExploreSeedNodes(daoSortedReqVoPlan, addressRequest(address));
        log.info("daoExploreSeedNodes plan return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 1, returnVo.getDataList().size()); // 5
        Assert.assertEquals("成功", new Integer(2), returnVo.getDataList().get(0).getDaoId());

        // 过滤指定类型的dao
        DaoSortedReqVo daoSortedReqVoTokenType = new DaoSortedReqVo();
        List<String> inputTokenTypes = new ArrayList<>();
        inputTokenTypes.add("USDC");
        daoSortedReqVoTokenType.setInputTokenTypes(inputTokenTypes);
        returnVo = daoController.daoExploreSeedNodes(daoSortedReqVoTokenType, addressRequest(address));
        log.info("daoExploreSeedNodes USDC return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
        Assert.assertEquals("接口数据量", 2, returnVo.getDataList().size()); // 2,20

        // 过滤全部条件的
        DaoSortedReqVo daoSortedReqVoAll = new DaoSortedReqVo();
        daoSortedReqVoAll.setTopUpMode(true);
        daoSortedReqVoAll.setWithIncentivePlan(true);
        daoSortedReqVoAll.setInputTokenTypes(inputTokenTypes);
        returnVo = daoController.daoExploreSeedNodes(daoSortedReqVoAll, addressRequest(address));
        log.info("daoExploreSeedNodes all filter return data:{}", JacksonUtil.obj2json(returnVo.getDataList()));
        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(), returnVo.getResultCode());
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