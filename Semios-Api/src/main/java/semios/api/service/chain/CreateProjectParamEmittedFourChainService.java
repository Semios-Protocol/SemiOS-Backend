package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.chain.*;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.request.SubscribeRequestDto;
import semios.api.model.dto.response.NewProjectUriDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.service.order.CreateDaoEventService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.merkle.MerkleTree;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * 1.4 新建dao抛出事件 CreateProjectParamEmitted
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-11-10
 **/
@Slf4j
@Service
public class CreateProjectParamEmittedFourChainService implements SubscriberChainService {
    // v2 https://goerli.etherscan.io/tx/0x001a4a459e1670e2988f1b73efe7cb40e35a7b84726c4a87bf19db9f5b5fd6ea#eventlog

    private static final RestTemplate REST_TEMPLATE = new RestTemplate();
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private CommonService commonService;
    @Autowired
    private ISubscribeService subscribeService;
    @Autowired
    private IDaoStrategyService daoStrategyService;
    @Autowired
    private IWhiteListService whiteListService;
    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;
    @Autowired
    private CreateDaoEventService createDaoEventService;
    @Autowired
    private S3Service s3Service;

    @Value("${dao_default_logo}")
    private String daoDefaultLogo;
    @Value("${dao_default_bgbanner}")
    private String daoDefaultBgbanner;

    public static void main(String[] args) {

//        String dataStr = "0x00000000000000000000000000000000000000000000000000000000009e3f270000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000271000000000000000000000000083770f065a60c874c5adf211d4531d375e41298400000000000000000000000000000000000000000000000000000000000002ee000000000000000000000000000000000000000000000000000000000000016500000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005d68747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f64616f2f4337584f4f38467074376b5a53654b687148735137303539352e6a736f6e000000";
//
//        List<String> projectDataList = CommonUtil.splitBy32Bytes(dataStr);
//        // uint256 : 53
//        String start_prb = CommonUtil.hexToTenString(projectDataList.get(0));
//        // uint256 : 90
//        String mintable_rounds = CommonUtil.hexToTenString(projectDataList.get(1));
//        // uint256 : 0
//        //String floor_price_rank = CommonUtil.hexToTenString(projectDataList.get(2));
//        // uint256 : 1000
//        String max_nft_amount = CommonUtil.hexToTenString(projectDataList.get(2));
//        // uint256 : 2
//        String index = CommonUtil.hexToTenString(projectDataList.get(5));
//        // uint256 :// 10000000000000000000000000000
//        String erc20_total_supply = CommonUtil.hexToTenString(projectDataList.get(7));
//
//        System.out.println(start_prb);
//        System.out.println("===========");


        String data = "0x3db490359d16882215de768796011873af9b13de564b199750bcca2c1b6620fd0000000000000000000000006c002d4690eb054ab86c360968219250c3abce99000000000000000000000000bcc2c9ba85e8dc65e2461093a04bdecfefdc2b6f000000000000000000000000ef9efeb3253c1168a416e85bce10f2babdf5adb700000000000000000000000000000000000000000000000000000000000003a00000000000000000000000000000000000000000000000000000000000000520000000000000000000000000000000000000000000000000000000000000072000000000000000000000000000000000000000000000000000000000000007a00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000086000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004e2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008e00000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000fa0000000000000000000000000000000000000000000000000000000000002616000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000fa000000000000000000000000000000000000000000000000000000000000261600000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000009c4000000000000000000000000000000000000000000000000000000000000138800000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000013880000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003c00000000000000000000000000000000000000000000015458b553d75e817d8000000000000000000000000000000000000000000000000000005af3107a4000000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000001f400000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005c68747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f64616f2f314f6e4f306d655638554436676d73356d5a4131383233392e6a736f6e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000016000000000000000000000000000000000000000000000000000000000000001a000000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d700000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d7000000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d700000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d700000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d7000000000000000000000000000000000000000000000000000000000000000500000000000000000000000000000000000000000000000000000000000000010000000000000000000000007af09a11cda4e696006a950309fe371d52b801d7000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000030bb215cb0d9ba8bd2b1dd0927850c32ae765793e6ed1b6e5253fb1f34fea7ea800000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000120000000000000000000000000000000000000000000000000000000000000008768747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f63616e7661732f306262323135636230643962613862643262316464303932373835306333326165373635373933653665643162366535323533666231663334666561376561382e6a736f6e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000b7768697465206c69737420000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String feePool = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String erc721_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));

        /**
         * 1. 解析参数
         * 2. 查询uri 获取NewProjectUriDto信息
         * 3. 查询projectInfo信息
         * 4. 查询 FEE_RECEIPT
         * 5. 查询 DAO_SYMBOL NAME
         * 6. 查询 NAME
         * 7. 查询 PROJECT_PRICE
         * 8. 构建dao信息 保存dao信息
         * 9. 刷新uri信息
         * 10.订阅dao相关的订阅信息
         */

        //1. 解析参数 daoMetadataParam
        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(4)))) >> 5;
        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(5)))) >> 5;
        int c = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(6)))) >> 5;
        int d = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(7)))) >> 5;
        int e = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(8)))) >> 5;
        int f = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(9)))) >> 5;
        //dao信息
        String daoMetadataParam = "";
        //dao 白名单信息
        String whitelist = "";
        //dao黑名单信息
        String blacklist = "";
        // dao铸造上限信息
        String daoMintCapParam = "";
        // nftMinterCapInfo 设置erc721白名单
        String nftMinterCapInfo = "";
        // nftMinterCapIdInfo   设置erc721下某nft白名单
        String nftMinterCapIdInfo = "";

        for (int i = a; i < b; i++) {
            daoMetadataParam += dataList.get(i);
        }
        log.info("daoMetadataParam:" + daoMetadataParam);
        for (int i = b; i < c; i++) {
            whitelist += dataList.get(i);
        }
        log.info("whitelist:" + whitelist);
        for (int i = c; i < d; i++) {
            blacklist += dataList.get(i);
        }
        log.info("blacklist:" + blacklist);
        for (int i = d; i < e; i++) {
            daoMintCapParam += dataList.get(i);
        }
        log.info("daoMintCapParam:" + daoMintCapParam);
        for (int i = e; i < f; i++) {
            nftMinterCapInfo += dataList.get(i);
        }
        log.info("nftMinterCapInfo:" + nftMinterCapInfo);
        for (int i = f; i < dataList.size(); i++) {
            nftMinterCapIdInfo += dataList.get(i);
        }
        log.info("nftMinterCapIdInfo:" + nftMinterCapIdInfo);

        TransactionDto transactionDto = new TransactionDto();
        transactionDto.setTransactionHash("1123");
        transactionDto.setBlockTime("123");

        Dao dao = new Dao();
        dao.setId(1);
        dao.setDaoUri("123");
        dao.setProjectId("123");
        dao.setDaoNumber(1);

        CreateProjectParamEmittedFourChainService createProjectParamEmittedFourChainService = new CreateProjectParamEmittedFourChainService();
        List<DaoStrategy> res = createProjectParamEmittedFourChainService.buildNftMinterCapIdInfo(transactionDto, nftMinterCapIdInfo, dao);

//        List<String> daoMetadataParamList = CommonUtil.splitBy32Bytes(daoMetadataParam);
//        String projectUri = CommonUtil.dynamicArgumentDecoding(daoMetadataParam, daoMetadataParamList.get(6), true);
//        log.info("projectUri:"+projectUri);
    }

    @Override
    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[CreateProjectParamEmittedFourChainService]transactionDto:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String feePool = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String erc721_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));

        /**
         * 1. 解析参数
         * 2. 查询uri 获取NewProjectUriDto信息
         * 3. 查询projectInfo信息
         * 4. 查询 FEE_RECEIPT
         * 5. 查询 DAO_SYMBOL NAME
         * 6. 查询 NAME
         * 7. 查询 PROJECT_PRICE
         * 8. 构建dao信息 保存dao信息
         * 9. 刷新uri信息
         * 10.订阅dao相关的订阅信息
         */

        //1. 解析参数 daoMetadataParam
        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(4)))) >> 5;
        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(5)))) >> 5;
        int c = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(6)))) >> 5;
        int d = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(7)))) >> 5;
        int e = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(8)))) >> 5;
        int f = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(9)))) >> 5;
        //dao信息
        String daoMetadataParam = "";
        //dao 白名单信息
        String whitelist = "";
        //dao黑名单信息
        String blacklist = "";
        // dao铸造上限信息
        String daoMintCapParam = "";
        // nftMinterCapInfo 设置erc721白名单
        String nftMinterCapInfo = "";
        // nftMinterCapIdInfo   设置erc721下某nft白名单
        String nftMinterCapIdInfo = "";

        for (int i = a; i < b; i++) {
            daoMetadataParam += dataList.get(i);
        }
        for (int i = b; i < c; i++) {
            whitelist += dataList.get(i);
        }
        for (int i = c; i < d; i++) {
            blacklist += dataList.get(i);
        }
        for (int i = d; i < e; i++) {
            daoMintCapParam += dataList.get(i);
        }
        for (int i = e; i < f; i++) {
            nftMinterCapInfo += dataList.get(i);
        }
        for (int i = f; i < dataList.size(); i++) {
            nftMinterCapIdInfo += dataList.get(i);
        }

        List<String> daoMetadataParamList = CommonUtil.splitBy32Bytes(daoMetadataParam);
        String startBlock = CommonUtil.hexToTenString(daoMetadataParamList.get(0));
        String mintableRounds = CommonUtil.hexToTenString(daoMetadataParamList.get(1));
        String duration = CommonUtil.hexToTenString(daoMetadataParamList.get(2));
        // 现在直接传值而非rank
        String floorPrice = CommonUtil.hexToTenString(daoMetadataParamList.get(3));
        //用户选择的最大铸造数量，从0开始编号，例如选择第一个则该值为0
        String maxNftRank = CommonUtil.hexToTenString(daoMetadataParamList.get(4));
        String royaltyFee = CommonUtil.hexToTenString(daoMetadataParamList.get(5));
        String projectUri = CommonUtil.dynamicArgumentDecoding(daoMetadataParam, daoMetadataParamList.get(6), true);
        String projectIndex = CommonUtil.hexToTenString(daoMetadataParamList.get(7));


        log.info("[CreateProjectParamEmittedFourChainService]uri:{}", projectUri);
        Dao dao = daoService.daoDetailByUri(projectUri);
        if (dao != null) {
            log.info("[CreateProjectParamEmittedFourChainService]dao is exits uri:{}", projectUri);
            throw new RuntimeException("dao is exits uri:" + projectUri);
        }

        NewProjectUriDto newProjectUriDto = REST_TEMPLATE.getForObject(projectUri, NewProjectUriDto.class);
        log.info("[CreateProjectParamEmittedFourChainService]uri return projectInfo:{}", JacksonUtil.obj2json(newProjectUriDto));
        if (newProjectUriDto == null || StringUtils.isBlank(newProjectUriDto.getName())) {
            log.error("[CreateProjectParamEmittedFourChainService] error newProjectUriDto is null transactionDto:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("newProjectUriDto is null");
        }

        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.PROJECT_INFO.getContractAddress());
        infuraCallRequestDto.setData(ContractMethodEnum.PROJECT_INFO.getMethodAddress() + projectId);

        // 查询合约的projectInfo
        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[CreateProjectParamEmittedFourChainService] error result:{}", result.getResultDesc());
            throw new RuntimeException("保存project查询信息失败");
        }
        log.info("[CreateProjectParamEmittedFourChainService]infura PROJECT_INFO return data:{}", result.getData());
        String projectInfoData = result.getData();
        List<String> projectDataList = CommonUtil.splitBy32Bytes(projectInfoData);
        // uint256 : 53
        String start_prb = CommonUtil.hexToTenString(projectDataList.get(0));
        // uint256 : 90
        String mintable_rounds = CommonUtil.hexToTenString(projectDataList.get(1));
        // uint256 : 0
        //String floor_price_rank = CommonUtil.hexToTenString(projectDataList.get(2));
        // uint256 : 1000
        String max_nft_amount = CommonUtil.hexToTenString(projectDataList.get(2));
        // uint256 : 2
        String index = CommonUtil.hexToTenString(projectDataList.get(5));
        // uint256 :// 10000000000000000000000000000
//        String erc20_total_supply = CommonUtil.hexToTenString(projectDataList.get(7));


        // newProjectUriDto.setName("d4a-"+index + "-" + dao.getDaoName().toLowerCase().replaceAll(" ", "-"));
        if (StringUtils.isNotBlank(royaltyFee)) {
            newProjectUriDto.setSeller_fee_basis_points(Integer.valueOf(royaltyFee));
        }
        //获取fee_recipient地址
        String fee_recipient = this.getFeeRecipient(projectId);
        newProjectUriDto.setFee_recipient(CommonUtil.addHexPrefixIfNotExist(fee_recipient));
        log.info("[CreateProjectParamEmittedFourChainService]uri return projectInfo:{}", JacksonUtil.obj2json(newProjectUriDto));
        //通过合约查询daoSymbol信息
        String daoSymbol = this.getDaoSymbol(erc20_token);

        //通过合约查询erc20Name信息
        String erc20Name = this.getErc20Name(erc20_token);

        //通过合约查询project price
        String priceData = getProjectPrice(projectId);

        //查询input参数看是否自动生成work
        // 获取transactionHash的参数input，从中获取调用方法的参数canvasId和projectId
        result = iSubscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
        log.info("[CreateProjectParamEmittedFourChainService] result:{}", result.getData());
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[CreateProjectParamEmittedFourChainService] error result:{}", result.getResultDesc());
            return;
        }
        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
        if (objectMap == null) {
            log.error("[CreateProjectParamEmittedFourChainService] objectMap is null result:{}", result.getData());
            return;
        }
        String input = (String) objectMap.get("input");
        input = input.substring(10);
        input = input.substring(0, 19 * 64);
        List<String> inputDataList = CommonUtil.splitBy32Bytes(input);
        String addWork = CommonUtil.hexToTenString(inputDataList.get(inputDataList.size() - 2));

        dao = new Dao();
        dao.setDaoSymbol(daoSymbol);
        dao.setErc20Name(erc20Name);
        if (StringUtils.isNotBlank(index)) {
            dao.setDaoNumber(Integer.valueOf(index));
        }
        dao.setDaoStatus(DaoStatusEnum.NOT_STARTED.getStatus());
        // 等于0的时候直接开始
        if ("0".equals(startBlock)) {
            dao.setDaoStatus(DaoStatusEnum.STARTED.getStatus());
        }
        dao.setSyncDex(1);
        dao.setBlockTime(transactionDto.getBlockTime());
        dao.setTransactionHash(transactionDto.getTransactionHash());
        dao.setBlockNumber(transactionDto.getBlockNumber());
        dao.setProjectId(projectId);
        dao.setReceivedToken(BigDecimal.ZERO);
        dao.setUnclaimedToken(BigDecimal.ZERO);
//        dao.setFeePool(feePool);
        //feepool指的是redeem那个池子
        dao.setDaoRedeemPool(feePool);
        dao.setErc20Token(erc20_token);
        dao.setErc721Token(erc721_token);
        dao.setRoyaltyFee(royaltyFee);
        dao.setBasicDao(BasicDaoEnum.PROTO_DAO.getBasicType());
        // 1.8.5之后的都可以修改
        dao.setDaoVersion(3);


        dao.setDaoName(newProjectUriDto.getName().replaceFirst("ProtoDao ", ""));
        dao.setDaoManitesto(newProjectUriDto.getManitesto());
        dao.setDaoDescription(newProjectUriDto.getDescription());
        dao.setSocialLinks(newProjectUriDto.getSocialLinks());
        dao.setWorkUrlSuffix(".png");
        LocalDate dateParam;
        try {
            DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            dateParam = LocalDate.parse(newProjectUriDto.getDaoStart_date(), df);
        } catch (Exception ex) {
            log.error("[CreateProjectParamEmittedFourChainService] error dateParam is null daoStartDate:{}",
                    newProjectUriDto.getDaoStart_date());
            dateParam = null;
        }

        dao.setDaoStartDate(dateParam);
        dao.setAddWork("0".equals(addWork) ? 1 : 0);
        dao.setNeedMintableWork("0".equals(addWork) ? 1 : 0);

        if (StringUtils.isNotBlank(start_prb)) {
            dao.setDaoStartBlock(start_prb);
        }
        if (StringUtils.isNotBlank(duration)) {
            dao.setDuration(duration);
        }
        if (StringUtils.isNotBlank(max_nft_amount)) {
            dao.setTotalNftCasting(Integer.valueOf(max_nft_amount));
        }
        if (StringUtils.isNotBlank(mintable_rounds)) {
            dao.setDaoMintWindow(Integer.valueOf(mintable_rounds));
            dao.setRemainingMintWindow(Integer.valueOf(mintable_rounds));
        }


        // 注释原因：放在CreateContinuousProjectParamEmittedChainService.java事件中监听计算，此方法不知道有没有开启erc20支付，如果未开启，需要使用eth支持，不用除decimals
//        if (StringUtils.isNotBlank(priceData)) {
//            String decimals = commonService.erc20Decimals(dao.getErc20Token());
//            // 如果erc20支付未开启，不用计算...
//            if (StringUtils.isNotBlank(decimals)) {
//                dao.setDaoFloorPrice(new BigDecimal(priceData).divide(new BigDecimal("10").pow(Integer.parseInt(decimals))));
//                dao.setCanvasFloorPrice(new BigDecimal(priceData).divide(new BigDecimal("10").pow(Integer.parseInt(decimals))));
//            } else {
//                dao.setDaoFloorPrice(new BigDecimal(priceData).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//                dao.setCanvasFloorPrice(new BigDecimal(priceData).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//            }
//        }

        if (StringUtils.isNotBlank(royaltyFee)) {
            DaoCreateFeeEnum daoCreateFeeEnum = DaoCreateFeeEnum
                    .getDaoCreateFeeEnumByActualValue(Integer.parseInt(royaltyFee) - ProtoDaoConstant.MINT_D4A_FEE_RATIO);
            if (daoCreateFeeEnum != null) {
                dao.setDaoCreateFee(Float.parseFloat(String.valueOf(daoCreateFeeEnum.getShowValue())));
            }
        }

        dao.setOpenseaLink(newProjectUriDto.getOpensea_link());
        dao.setTwitterLink(newProjectUriDto.getTwitter_link());
        dao.setDiscordLink(newProjectUriDto.getDiscord_link());
        dao.setOwnerAddress(newProjectUriDto.getUser_address());

        dao.setDaoUri(newProjectUriDto.getUri());
        if (StringUtils.isNotBlank(newProjectUriDto.getLogo())) {
            dao.setDaoLogoUrl(newProjectUriDto.getLogo());
        } else {
            dao.setDaoLogoUrl(daoDefaultLogo);
        }
        if (StringUtils.isNotBlank(newProjectUriDto.getBg_banner())) {
            dao.setDaoBgBanner(newProjectUriDto.getBg_banner());
        } else {
            dao.setDaoBgBanner(daoDefaultBgbanner);
        }
        dao.setDrbNumber(Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND));
        dao.setCurrentRound(commonService.getDaoCurrentRound(projectId));   // 避免订阅时，找不到sub id的偶发问题。 现查dao的周期


        //erc20 发放方式、价格变动及相关参数设置  templateParam
        buildTemplateParam(transactionDto, dao);


        List<DaoStrategy> daoStrategyList = new ArrayList<>();
        //更新白名单信息 这里不处理，在WhitelistModifiedChainService服务里处理，创建dao的时候同时会触发这个事件
//        List<DaoStrategy> daoWhiteListStrategyList = buildWhitelist(transactionDto, whitelist, dao);
//        if (daoWhiteListStrategyList != null && daoWhiteListStrategyList.size() > 0) {
//            daoStrategyList.addAll(daoWhiteListStrategyList);
//        }

        //更新黑名单信息 blacklist
        List<DaoStrategy> daoBlackListStrategyList = buildBlacklist(transactionDto, blacklist, dao);
        if (daoBlackListStrategyList != null && daoBlackListStrategyList.size() > 0) {
            daoStrategyList.addAll(daoBlackListStrategyList);
        }
        //更新铸造上限 daoMintCapParam
        List<DaoStrategy> daoMintCapStrategyList = buildDaoMintCapParam(transactionDto, daoMintCapParam, dao);

        if (daoMintCapStrategyList != null && !daoMintCapStrategyList.isEmpty()) {
            daoStrategyList.addAll(daoMintCapStrategyList);
        }

        //nftMinterCapInfo
        List<DaoStrategy> nftMinterCapInfoStrategyList = buildNftMinterCapInfo(transactionDto, nftMinterCapInfo, dao);

        if (!nftMinterCapInfoStrategyList.isEmpty()) {
            daoStrategyList.addAll(nftMinterCapInfoStrategyList);
        }


        // nftMinterCapIdInfo 是否限制erc721下的tokenId铸造白名单
        List<DaoStrategy> nftMinterCapIdInfoStrategyList = buildNftMinterCapIdInfo(transactionDto, nftMinterCapIdInfo, dao);

        if (!nftMinterCapIdInfoStrategyList.isEmpty()) {
            daoStrategyList.addAll(nftMinterCapIdInfoStrategyList);
        }


        //basicDaoParam 信息先不处理
        //actionType 它是合约调方法的一个判断符号，现在所有的创建应该都是默认20
        // String actionType = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));

        //更新dao收益分配 allRatioForFundingParam
        buildAllRatioForFundingParam(transactionDto, dao);

        //存储dao信息
        daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);

        //0914 创建dao的drb，如果没有铸造，会少计算一个drb的问题
        if (dao.getRoyaltyTokenLotteryMode() != null && dao.getRoyaltyTokenLotteryMode() == 1) {
            DaoDrbStatistics daoDrbStatistics1 = new DaoDrbStatistics();
            daoDrbStatistics1.setFloorPrice(dao.getDaoFloorPrice());
            daoDrbStatistics1.setDaoId(dao.getId());
            daoDrbStatistics1.setStatus(StatisticsStatusEnum.WJS.getStatus());
            daoDrbStatistics1.setDrbVol(BigDecimal.ZERO);
            daoDrbStatistics1.setDrbVolExTax(BigDecimal.ZERO);

            daoDrbStatistics1.setDrbNumber(0);  // 初始从0开始
            log.info("[CreateProjectParamEmittedFourChainService] daoId:{} start add DaoDrbStatistics daoStatus:{}", dao.getId(), dao.getDaoStatus());
            daoDrbStatisticsService.save(daoDrbStatistics1);
        }

        // 刷新uri
        this.freshProjectUri(projectUri, newProjectUriDto);

        // 订阅
        this.subscribeForNewProject(dao, transactionDto);

        // 事件排序顺序订阅
        createDaoEventService.handleTrade(transactionDto);
    }

    /**
     * erc20 发放方式、价格变动及相关参数设置
     *
     * @param transactionDto
     * @param dao
     */
    private void buildTemplateParam(TransactionDto transactionDto, Dao dao) {
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        //指数变价传 0，线性变价传 1
        String priceTemplateType = CommonUtil.hexToTenString(dataList.get(10));
        //变价的系数，如果选择了指数变价，则 priceFactor 传翻倍的系数  如果选择了线性变价，则传价格变动时，每次增加或者减少的价格数量，
        String priceFactor = CommonUtil.hexToTenString(dataList.get(11));
        //ERC20 奖励发放类型 线性发放奖励传 0，奖励发放指数衰减传 1  该参数应该传2（枚举类型）
        String rewardTemplateType = CommonUtil.hexToTenString(dataList.get(12));
        //奖励发放衰减的系数  例如均匀发放，则传 0,每次减少发放 10000 个 token，则传 10000 * 1e18  // 该参数已废弃，随便传参数即可
        String rewardDecayFactor = CommonUtil.hexToTenString(dataList.get(13));
        //衰减周期长度，默认传 1，表示一个周期中有 1 个 DRB
//        String rewardDecayLife = CommonUtil.hexToTenString(dataList.get(17));
        //是否累计奖池（乐透模式），是则传 true，不是传 false
        String isProgressiveJackpot = CommonUtil.hexToTenString(dataList.get(14));

        dao.setCanvasPriceFluctuationMethod(Integer.valueOf(priceTemplateType));
        dao.setFluctuationMethodFactor(new BigDecimal(priceFactor));
        dao.setRoyaltyTokenGenerationMethod(Integer.valueOf(rewardTemplateType));
        dao.setRoyaltyTokenGenerationFactory(new BigDecimal(rewardDecayFactor));
        dao.setRoyaltyTokenLotteryMode(Integer.valueOf(isProgressiveJackpot));

//        daoService.updateById(dao);
    }

    /**
     * 更新dao收益分配 allRatioForFundingParam
     *
     * @param transactionDto
     * @param dao
     */
    private void buildAllRatioForFundingParam(TransactionDto transactionDto, Dao dao) {

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        //DAO Price Reserve Ratio
        String canvasCreatorMintFeeRatio = CommonUtil.hexToTenString(dataList.get(17));// Builder Mint Fee
        String assetPoolMintFeeRatio = CommonUtil.hexToTenString(dataList.get(18));// SubDAO Mint Fee
        String redeemPoolMintFeeRatio = CommonUtil.hexToTenString(dataList.get(19));// MainDAO Mint Fee
        String treasuryMintFeeRatio = CommonUtil.hexToTenString(dataList.get(20));// treasury Mint Fee Ratio
        // 以上三个参数加 PAO Mint Fee 等于 10000


        // Fixed Price Reserve Ratio
        String canvasCreatorMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(21));// Builder Mint Fee
        String assetPoolMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(22));// SubDAO Mint Fee
        String redeemPoolMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(23));// MainDAO Mint Fee
        String treasuryMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(24));// treasury Mint Fee Ratio Fiat Price
        // 以上三个参数加 PDAO Mint Fee 等于 10000

        //Royalty Token
        String minterERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(25));// Minter Reward    合约改名字为：minterOutputRewardRatio
        String canvasCreatorERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(26));// Builder Reward  合约改名字为：canvasCreatorOutputRewardRatio
        String daoCreatorERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(27));// SubDAO Starter Reward  合约改名字为：daoCreatorOutputRewardRatio
        // 以上三个参数加 PDAO Reward 等于 10000

        // ETH
        String minterETHRewardRatio = CommonUtil.hexToTenString(dataList.get(28));// Minter Reward  合约改名字为：minterInputRewardRatio
        String canvasCreatorETHRewardRatio = CommonUtil.hexToTenString(dataList.get(29));// Builder Reward  合约改名字为：canvasCreatorInputRewardRatio
        String daoCreatorETHRewardRatio = CommonUtil.hexToTenString(dataList.get(30));// SubDAO Starter Reward  合约改名字为：daoCreatorInputRewardRatio
        // 以上三个参数加 PDAO Reward 等于 10000

        //DAO Price Reserve Ratio
        DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
        }
        daoRoyaltyToken.setDaoReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorERC20RewardRatio));
        daoRoyaltyToken.setCanvasReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorERC20RewardRatio));
        daoRoyaltyToken.setMinterReward(ProtoDaoCommonUtil.strToBigDecimal(minterERC20RewardRatio));
        daoRoyaltyToken.setD4aReward(new BigDecimal("100").subtract(daoRoyaltyToken.getDaoReward()).subtract(daoRoyaltyToken.getCanvasReward()).subtract(daoRoyaltyToken.getMinterReward()));

        //ETH
        DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);
        }
        daoEthRoyaltyToken.setMinterETHReward(ProtoDaoCommonUtil.strToBigDecimal(minterETHRewardRatio));
        daoEthRoyaltyToken.setCanvasCreatorETHReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorETHRewardRatio));
        daoEthRoyaltyToken.setDaoCreatorETHReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorETHRewardRatio));
        daoEthRoyaltyToken.setD4aReward(new BigDecimal("100").subtract(daoEthRoyaltyToken.getMinterETHReward()).subtract(daoEthRoyaltyToken.getCanvasCreatorETHReward()).subtract(daoEthRoyaltyToken.getDaoCreatorETHReward()));

        // 非一口价
        DaoReserveRatio daoReserveRatio = new DaoReserveRatio(true);
        if (StringUtils.isNotBlank(dao.getFixedReserveRatio())) {
            daoReserveRatio = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
        }
        daoReserveRatio.setCanvasMintFee(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorMintFeeRatio));
        daoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(assetPoolMintFeeRatio));
        daoReserveRatio.setRedeemPoolMintFee(ProtoDaoCommonUtil.strToBigDecimal(redeemPoolMintFeeRatio));
        daoReserveRatio.setD4aMintFee(new BigDecimal("100").subtract(daoReserveRatio.getDaoMintFee()).subtract(daoReserveRatio.getCanvasMintFee()).subtract(daoReserveRatio.getRedeemPoolMintFee()));
        // 一口价
        DaoReserveRatio fixedDaoReserveRatio = new DaoReserveRatio(false);
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            fixedDaoReserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
        }
        fixedDaoReserveRatio.setCanvasMintFee(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorMintFeeRatioFiatPrice));
        fixedDaoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(assetPoolMintFeeRatioFiatPrice));
        fixedDaoReserveRatio.setRedeemPoolMintFee(ProtoDaoCommonUtil.strToBigDecimal(redeemPoolMintFeeRatioFiatPrice));
        fixedDaoReserveRatio.setD4aMintFee(new BigDecimal("100").subtract(fixedDaoReserveRatio.getDaoMintFee()).subtract(fixedDaoReserveRatio.getCanvasMintFee()).subtract(fixedDaoReserveRatio.getRedeemPoolMintFee()));


        dao.setRoyaltyToken(JacksonUtil.obj2json(daoRoyaltyToken));
        dao.setEthRoyaltyToken(JacksonUtil.obj2json(daoEthRoyaltyToken));
        dao.setUnfixedReserveRatio(JacksonUtil.obj2json(daoReserveRatio));
        dao.setFixedReserveRatio(JacksonUtil.obj2json(fixedDaoReserveRatio));

        log.info("[DaoRatioSetChainService] daoId:{} royaltyToken:{}", dao.getId(), dao.getRoyaltyToken());
        log.info("[DaoRatioSetChainService] daoId:{} unfixedReserveRatio:{}", dao.getId(),
                dao.getUnfixedReserveRatio());
        log.info("[DaoRatioSetChainService] daoId:{} fixedReserveRatio:{}", dao.getId(), dao.getFixedReserveRatio());
//        daoService.updateById(dao);
    }

    /**
     * 更新铸造上限
     *
     * @param transactionDto
     * @param daoMintCapParam
     * @param dao
     */
    private List<DaoStrategy> buildDaoMintCapParam(TransactionDto transactionDto, String daoMintCapParam, Dao dao) {

        String data = CommonUtil.removeHexPrefixIfExists(daoMintCapParam);
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String mintCap = CommonUtil.hexToTenString(dataList.get(0));

        String capSize = CommonUtil.hexToTenString(dataList.get(2));
        List<DesignatedCap> designatedCaps = null;
        if (StringUtils.isNotBlank(capSize) && Integer.parseInt(capSize) > 0) {
            designatedCaps = new ArrayList<>(Integer.parseInt(capSize));
            for (int i = 0; i < Integer.parseInt(capSize); i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 3))).toLowerCase();
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 4));
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        designatedCaps.add(designatedCap);
                    }
                }
            }
        }
        List<DaoStrategy> daoStrategyList = new ArrayList<>();
        DaoStrategy newDaoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
        if (newDaoStrategy == null) {
            newDaoStrategy = new DaoStrategy();
            newDaoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            newDaoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
            newDaoStrategy.setProofId(dao.getId());
            newDaoStrategy.setDaoUri(dao.getDaoUri());
            newDaoStrategy.setDaoId(dao.getId());
            newDaoStrategy.setProjectId(dao.getProjectId());
            newDaoStrategy.setDaoNumber(dao.getDaoNumber());
        } else {
            log.info("[MintCapAddedChainService] daoId:{} origin daoStrategy:{}", dao.getId(),
                    JacksonUtil.obj2json(newDaoStrategy));
        }

        newDaoStrategy.setTransactionHash(transactionDto.getTransactionHash());
        newDaoStrategy.setBlockTime(transactionDto.getBlockTime());

        if (designatedCaps != null && !designatedCaps.isEmpty()) {
            newDaoStrategy.setOriginAddress(JacksonUtil.obj2json(designatedCaps));
            if (dao.getMintCap() == null || dao.getMintCap() == 0) {
                dao.setMintCap(1);
            }
        } else {
            newDaoStrategy.setOriginAddress(null);
            if (dao.getMintCap() != null && dao.getMintCap() == 1) {
                dao.setMintCap(0);
            }
        }

        daoStrategyList.add(newDaoStrategy);

        if (StringUtils.isNotBlank(mintCap)) {
            dao.setGlobalMintCap(Integer.parseInt(mintCap));
        }

        log.info("[MintCapAddedChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
                JacksonUtil.obj2json(daoStrategyList));
        return daoStrategyList;
//        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
//        log.info("[MintCapAddedChainService] daoId:{} return i:{}", dao.getId(), i);
    }

    /**
     * 更新Erc721铸造上限
     *
     * @param transactionDto
     * @param nftMinterCapInfo
     * @param dao
     */
    private List<DaoStrategy> buildNftMinterCapInfo(TransactionDto transactionDto, String nftMinterCapInfo, Dao dao) {

        String data = CommonUtil.removeHexPrefixIfExists(nftMinterCapInfo);
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String nftMinterCapSize = CommonUtil.hexToTenString(dataList.get(0));
        Integer nftMinterCapCount = Integer.parseInt(nftMinterCapSize);
        List<DesignatedCap> nftDesignatedCaps = null;
        if (StringUtils.isNotBlank(nftMinterCapSize) && Integer.parseInt(nftMinterCapSize) > 0) {
            nftDesignatedCaps = new ArrayList<>();
            for (int i = 0; i < nftMinterCapCount; i++) {
                DesignatedCap designatedCap = new DesignatedCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 1))).toLowerCase();
                designatedCap.setAccount(account);

                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 2));
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        nftDesignatedCaps.add(designatedCap);
                    }
                }
            }
        }

        DaoStrategy nftMinterStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
        if (nftMinterStrategy == null) {
            nftMinterStrategy = new DaoStrategy();
            nftMinterStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            nftMinterStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
            nftMinterStrategy.setProofId(dao.getId());
            nftMinterStrategy.setDaoUri(dao.getDaoUri());
            nftMinterStrategy.setDaoId(dao.getId());
            nftMinterStrategy.setProjectId(dao.getProjectId());
            nftMinterStrategy.setDaoNumber(dao.getDaoNumber());
        } else {
            log.info("[buildNftMinterCapInfo] daoId:{} HIGH_PRIORITY_ERC721 origin daoStrategy:{}", dao.getId(),
                    JacksonUtil.obj2json(nftMinterStrategy));
        }

        nftMinterStrategy.setTransactionHash(transactionDto.getTransactionHash());
        nftMinterStrategy.setBlockTime(transactionDto.getBlockTime());

        if (nftDesignatedCaps != null && !nftDesignatedCaps.isEmpty()) {
            nftMinterStrategy.setOriginAddress(JacksonUtil.obj2json(nftDesignatedCaps));
            if (dao.getErc721MintCap() == null || dao.getErc721MintCap() == 0) {
                dao.setId(dao.getId());
                dao.setErc721MintCap(1);
            }
        } else {
            nftMinterStrategy.setOriginAddress(null);
            if (dao.getErc721MintCap() != null && dao.getErc721MintCap() == 1) {
                dao.setId(dao.getId());
                dao.setErc721MintCap(0);
            }
        }

        List<DaoStrategy> daoStrategyList = new ArrayList<>();
        daoStrategyList.add(nftMinterStrategy);

        log.info("[buildNftMinterCapInfo] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
                JacksonUtil.obj2json(daoStrategyList));
        return daoStrategyList;
    }

    /**
     * 更新Erc721下的nft铸造上限
     *
     * @param transactionDto
     * @param nftMinterCapInfo
     * @param dao
     */
    private List<DaoStrategy> buildNftMinterCapIdInfo(TransactionDto transactionDto, String nftMinterCapInfo, Dao dao) {

        String data = CommonUtil.removeHexPrefixIfExists(nftMinterCapInfo);
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String nftMinterCapSize = CommonUtil.hexToTenString(dataList.get(0));
        Integer nftMinterCapCount = Integer.parseInt(nftMinterCapSize);
        List<DesignatedNftCap> nftDesignatedCaps = null;
        if (StringUtils.isNotBlank(nftMinterCapSize) && Integer.parseInt(nftMinterCapSize) > 0) {
            nftDesignatedCaps = new ArrayList<>();
            for (int i = 0; i < nftMinterCapCount; i++) {
                DesignatedNftCap designatedCap = new DesignatedNftCap();
                String account = CommonUtil
                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3 * i + 1))).toLowerCase();
                designatedCap.setNftAddress(account);
                log.info("account:" + account);

                String tokenId = CommonUtil.hexToTenString(dataList.get(3 * i + 2));
                log.info("tokenId:" + tokenId);
                if (StringUtils.isNotBlank(tokenId)) {
                    designatedCap.setTokenId(Integer.parseInt(tokenId));
                }

                String cap = CommonUtil.hexToTenString(dataList.get(3 * i + 3));
                if (StringUtils.isNotBlank(cap)) {
                    designatedCap.setNftMintCap(Integer.parseInt(cap));
                    if (Integer.parseInt(cap) > 0) {
                        nftDesignatedCaps.add(designatedCap);
                    }
                }
            }
        }
        DaoStrategy nftMinterStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType());
        if (nftMinterStrategy == null) {
            nftMinterStrategy = new DaoStrategy();
            nftMinterStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            nftMinterStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721_NFT.getType());
            nftMinterStrategy.setProofId(dao.getId());
            nftMinterStrategy.setDaoUri(dao.getDaoUri());
            nftMinterStrategy.setDaoId(dao.getId());
            nftMinterStrategy.setProjectId(dao.getProjectId());
            nftMinterStrategy.setDaoNumber(dao.getDaoNumber());
        } else {
            log.info("[buildNftMinterCapInfo] daoId:{} HIGH_PRIORITY_ERC721_NFT origin daoStrategy:{}", dao.getId(),
                    JacksonUtil.obj2json(nftMinterStrategy));
        }

        nftMinterStrategy.setTransactionHash(transactionDto.getTransactionHash());
        nftMinterStrategy.setBlockTime(transactionDto.getBlockTime());

        if (nftDesignatedCaps != null && !nftDesignatedCaps.isEmpty()) {
            nftMinterStrategy.setOriginAddress(JacksonUtil.obj2json(nftDesignatedCaps));
            if (dao.getErc721MintCapId() == null || dao.getErc721MintCapId() == 0) {
                dao.setId(dao.getId());
                dao.setErc721MintCapId(1);
            }
        } else {
            nftMinterStrategy.setOriginAddress(null);
            if (dao.getErc721MintCapId() != null && dao.getErc721MintCapId() == 1) {
                dao.setId(dao.getId());
                dao.setErc721MintCapId(0);
            }
        }

        List<DaoStrategy> daoStrategyList = new ArrayList<>();
        daoStrategyList.add(nftMinterStrategy);

        log.info("[buildNftMinterCapInfo] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
                JacksonUtil.obj2json(daoStrategyList));
        return daoStrategyList;
    }

    /**
     * 更新黑名单信息
     *
     * @param transactionDto
     * @param blacklist
     * @param dao
     */
    private List<DaoStrategy> buildBlacklist(TransactionDto transactionDto, String blacklist, Dao dao) {

        List<String> dataList = CommonUtil.splitBy32Bytes(blacklist);

        List<String> minterNftHolderPass = new ArrayList<>();
        List<String> canvasCreatorNftHolderPass = new ArrayList<>();
        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(0)))) >> 5;
        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(1)))) >> 5;
        for (int i = a + 1; i < b; i++) {
            String mintNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
            if (ProtoDaoConstant.ZERO_ADDRESS.equals(mintNftHolder)) {
                continue;
            }
            minterNftHolderPass.add(mintNftHolder);
        }


        for (int i = b + 1; i < dataList.size(); i++) {
            String canvasCreatorNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
            if (ProtoDaoConstant.ZERO_ADDRESS.equals(canvasCreatorNftHolder)) {
                continue;
            }
            canvasCreatorNftHolderPass.add(canvasCreatorNftHolder);
        }

        if (minterNftHolderPass.isEmpty() && canvasCreatorNftHolderPass.isEmpty()) {
            return null;
        }

        List<DaoStrategy> daoStrategyList = new ArrayList<>();
        if (!minterNftHolderPass.isEmpty()) {
            DaoStrategy daoStrategy = new DaoStrategy();
            daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
            daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            daoStrategy.setOriginAddress(String.join(",", minterNftHolderPass));
            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoStrategy.setBlockTime(transactionDto.getBlockTime());
            daoStrategy.setDaoUri(dao.getDaoUri());
            daoStrategy.setDaoId(dao.getId());
            daoStrategy.setProjectId(dao.getProjectId());
            daoStrategy.setDaoNumber(dao.getDaoNumber());
            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoStrategy.setBlockTime(transactionDto.getBlockTime());
            daoStrategyList.add(daoStrategy);

            dao.setMinterWorksBlacklist(DaoBlackListEnum.OPEN.getStatus());
        }


        if (!canvasCreatorNftHolderPass.isEmpty()) {


            DaoStrategy daoStrategy = new DaoStrategy();
            daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
            daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
            daoStrategy.setOriginAddress(String.join(",", canvasCreatorNftHolderPass));
            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoStrategy.setBlockTime(transactionDto.getBlockTime());
            daoStrategy.setDaoUri(dao.getDaoUri());
            daoStrategy.setDaoId(dao.getId());
            daoStrategy.setProjectId(dao.getProjectId());
            daoStrategy.setDaoNumber(dao.getDaoNumber());
            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoStrategy.setBlockTime(transactionDto.getBlockTime());

            daoStrategyList.add(daoStrategy);

            dao.setCanvasCreatedBlacklist(DaoBlackListEnum.OPEN.getStatus());

        }

        return daoStrategyList;
//        daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
    }

    /**
     * 更新白名单信息
     *
     * @param transactionDto
     * @param whitelist
     * @param dao
     */
    private List<DaoStrategy> buildWhitelist(TransactionDto transactionDto, String whitelist, Dao dao) {
        try {
            List<String> dataList = CommonUtil.splitBy32Bytes(whitelist);
            String minterMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
            String canvasCreatorMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(2));
            WhiteList whiteList = null;

            List<String> minterNftHolderPass = new ArrayList<>();
            List<String> canvasCreatorNftHolderPass = new ArrayList<>();
            int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(1)))) >> 5;
            int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(3)))) >> 5;
            for (int i = a; i < b; i++) {
                String mintNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
                if (ProtoDaoConstant.ZERO_ADDRESS.equals(mintNftHolder)) {
                    continue;
                }
                minterNftHolderPass.add(mintNftHolder);
            }


            for (int i = b; i < dataList.size(); i++) {
                String canvasCreatorNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
                if (ProtoDaoConstant.ZERO_ADDRESS.equals(canvasCreatorNftHolder)) {
                    continue;
                }
                canvasCreatorNftHolderPass.add(canvasCreatorNftHolder);
            }


            List<DaoStrategy> daoStrategyList = new ArrayList<>();

            log.info(
                    "[CreateProjectParamEmittedFourChainService] daoId:{} minterMerkleRoot:{} minterNftHolderPass:{} canvasCreatorMerkleRoot:{} canvasCreatorNftHolderPass:{}",
                    dao.getId(), minterMerkleRoot, JacksonUtil.obj2json(minterNftHolderPass), canvasCreatorMerkleRoot,
                    JacksonUtil.obj2json(canvasCreatorNftHolderPass));
            if (StringUtils.isNotBlank(minterMerkleRoot)) {
                if (minterMerkleRoot.equals(ProtoDaoConstant.ZERO_MERKLE_ROOT)) {
                    if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getMinterWorksWhitelist())
                            || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getMinterWorksWhitelist())) {
                        dao.setMinterWorksWhitelist(dao.getMinterWorksWhitelist() - 1);
                        // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
                    }
                } else {
                    // 查询merkleRoot
                    whiteList = whiteListService.selectByAddressAndRoot(dao.getOwnerAddress(), minterMerkleRoot);
                    if (whiteList == null) {
                        whiteList = new WhiteList();
                        whiteList.setUserAddress(dao.getOwnerAddress());
                        String originAddress = dao.getOwnerAddress();
                        List<String> list = Arrays.asList(originAddress);
                        MerkleTree mt = new MerkleTree(list);
                        mt.init();
                        String rootHash = mt.getRootHash();
                        if (!rootHash.equals(minterMerkleRoot.toLowerCase())) {
                            throw new RuntimeException(
                                    "diff root hash, front:" + minterMerkleRoot + ", end:" + rootHash);
                        }
                        // mintingWhiteList.setOriginAddress(daoWhiteListReqVo.getMintingOriginAddress());
                        whiteList.setOriginAddress(JacksonUtil.obj2json(list));
                        whiteList.setProof(JacksonUtil.obj2json(mt));
                        whiteList.setProofRootHash(minterMerkleRoot);
                        whiteListService.save(whiteList);
//                    throw new RuntimeException("CreateProjectParamEmittedFourChainService cannot find whiteList");
                    }

                    DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                            DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                    if (daoStrategy == null) {
                        daoStrategy = new DaoStrategy();
                        daoStrategy.setProofId(whiteList.getId());
                        daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
                        daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                        daoStrategy.setOriginAddress(whiteList.getOriginAddress()
                                .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
                        daoStrategy.setDaoUri(dao.getDaoUri());
                        daoStrategy.setDaoId(dao.getId());
                        daoStrategy.setProjectId(dao.getProjectId());
                        daoStrategy.setDaoNumber(dao.getDaoNumber());

                    } else {
                        daoStrategy.setOriginAddress(whiteList.getOriginAddress()
                                .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
                        daoStrategy.setProofId(whiteList.getId());
                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
                    }

                    if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getMinterWorksWhitelist())) {
                        dao.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS.getStatus());
                    }

                    if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getMinterWorksWhitelist())) {
                        dao.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
                    }

                    daoStrategyList.add(daoStrategy);
                    // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
                }
            }

            if (minterNftHolderPass.isEmpty()) {
                // if (minterNftHolderPass.get(0).equals(ProtoDaoConstant.ZERO_ADDRESS)) {
                if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getMinterWorksWhitelist())
                        || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getMinterWorksWhitelist())) {
                    dao.setMinterWorksWhitelist(dao.getMinterWorksWhitelist() - 2);
                    // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
                }
            } else {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
                if (daoStrategy == null) {
                    daoStrategy = new DaoStrategy();
                    daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
                    daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721.getType());
                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNftHolderPass));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                    daoStrategy.setDaoUri(dao.getDaoUri());
                    daoStrategy.setDaoId(dao.getId());
                    daoStrategy.setProjectId(dao.getProjectId());
                    daoStrategy.setDaoNumber(dao.getDaoNumber());

                } else {
                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNftHolderPass));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                }

                if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getMinterWorksWhitelist())) {
                    dao.setMinterWorksWhitelist(DaoWhiteListEnum.ERC721.getStatus());
                }

                if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getMinterWorksWhitelist())) {
                    dao.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
                }

                daoStrategyList.add(daoStrategy);
                // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
            }

            if (StringUtils.isNotBlank(canvasCreatorMerkleRoot)) {
                if (canvasCreatorMerkleRoot.equals(ProtoDaoConstant.ZERO_MERKLE_ROOT)) {
                    if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getCanvasCreatedWhitelist())
                            || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
                        dao.setCanvasCreatedWhitelist(dao.getCanvasCreatedWhitelist() - 1);
                        // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
                    }
                } else {
                    // 查询merkleRoot
                    WhiteList whiteListCanvas =
                            whiteListService.selectByAddressAndRoot(dao.getOwnerAddress(), canvasCreatorMerkleRoot);
                    if (whiteListCanvas == null && whiteList != null) {
                        whiteListCanvas = whiteList;
//                        throw new RuntimeException("CreateProjectParamEmittedFourChainService cannot find whiteList");
                    }

                    DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                            DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                    if (daoStrategy == null) {
                        daoStrategy = new DaoStrategy();
                        daoStrategy.setProofId(whiteListCanvas.getId());
                        daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
                        daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
                        daoStrategy.setOriginAddress(whiteListCanvas.getOriginAddress()
                                .substring(1, whiteListCanvas.getOriginAddress().length() - 1).replaceAll("\"", ""));
                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
                        daoStrategy.setDaoUri(dao.getDaoUri());
                        daoStrategy.setDaoId(dao.getId());
                        daoStrategy.setProjectId(dao.getProjectId());
                        daoStrategy.setDaoNumber(dao.getDaoNumber());

                    } else {
                        daoStrategy.setProofId(whiteListCanvas.getId());
                        daoStrategy.setOriginAddress(whiteListCanvas.getOriginAddress()
                                .substring(1, whiteListCanvas.getOriginAddress().length() - 1).replaceAll("\"", ""));
                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
                    }

                    if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
                        dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS.getStatus());
                    }

                    if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
                        dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
                    }

                    daoStrategyList.add(daoStrategy);
                    // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
                }
            }

            if (canvasCreatorNftHolderPass.isEmpty()) {
                // if (canvasCreatorNftHolderPass.equals(ProtoDaoConstant.ZERO_ADDRESS)) {
                if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())
                        || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
                    dao.setCanvasCreatedWhitelist(dao.getCanvasCreatedWhitelist() - 2);
                    // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
                }
            } else {
                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
                if (daoStrategy == null) {
                    daoStrategy = new DaoStrategy();
                    daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
                    daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721.getType());
                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNftHolderPass));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                    daoStrategy.setDaoUri(dao.getDaoUri());
                    daoStrategy.setDaoId(dao.getId());
                    daoStrategy.setProjectId(dao.getProjectId());
                    daoStrategy.setDaoNumber(dao.getDaoNumber());

                } else {
                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNftHolderPass));
                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
                }

                if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
                    dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ERC721.getStatus());
                }

                if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
                    dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
                }

                daoStrategyList.add(daoStrategy);
                // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
            }

            log.info("[CreateProjectParamEmittedFourChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
                    JacksonUtil.obj2json(daoStrategyList));
            dao.setDaoManitesto(dao.getDaoManitesto());
            dao.setDaoDescription(dao.getDaoDescription());
            dao.setFavoriteAmount(dao.getFavoriteAmount());


            return daoStrategyList;
//        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
//        log.info("[CreateProjectParamEmittedFourChainService] daoId:{} return i:{}", dao.getId(), i);
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService] daoId:{} Exception e:{}", dao.getDaoNumber(), e);
        }
        return null;
    }

    private String getFeeRecipient(String projectId) {
        try {
            InfuraCallRequestDto infuraCallRequestDto1 = new InfuraCallRequestDto();
            infuraCallRequestDto1.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto1.setTo(ContractMethodEnum.FEE_RECEIPT.getContractAddress());
            infuraCallRequestDto1.setData(ContractMethodEnum.FEE_RECEIPT.getMethodAddress() + projectId);

            // 调用查询使用数据集的user
            Result<String> resul1 = iSubscriptionService.infuraCall(infuraCallRequestDto1);
            log.info("[CreateProjectParamEmittedFourChainService]infura FEE_RECEIPT return data:{}", resul1.getData());
            if (resul1.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] FEE_RECEIPT error result:{}", resul1.getResultDesc());
                throw new RuntimeException("保存project查询信息失败");
            }

            String projectInfoData1 = resul1.getData();
            List<String> dataList1 = CommonUtil.splitBy32Bytes(projectInfoData1);
            String fee_recipient = CommonUtil.formatBytes32Address(dataList1.get(0));
            return fee_recipient;
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService]infura FEE_RECEIPT projectId:{} exception:{}", projectId, e);
        }
        return null;
    }

    /**
     * 查询dao daoSymbol信息
     *
     * @param erc20_token
     * @return
     */
    private String getDaoSymbol(String erc20_token) {
        try {
            InfuraCallRequestDto infuraCallRequestDto2 = new InfuraCallRequestDto();
            infuraCallRequestDto2.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto2.setTo(erc20_token);
            infuraCallRequestDto2.setData(ContractMethodEnum.DAO_SYMBOL.getMethodAddress());

            // 调用查询使用数据集的user
            Result<String> resul2 = iSubscriptionService.infuraCall(infuraCallRequestDto2);
            if (resul2.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] dao symbol error result:{}", resul2.getResultDesc());
                throw new RuntimeException("保存project查询symbol信息失败");
            }
            log.info("[CreateProjectParamEmittedFourChainService]infura symbol return data:{}", resul2.getData());
            String projectInfoData2 = CommonUtil.removeHexPrefixIfExists(resul2.getData());
            List<String> dataList2 = CommonUtil.splitBy32Bytes(projectInfoData2);
            String daoSymbol = CommonUtil.dynamicArgumentDecoding(projectInfoData2, dataList2.get(0), true);
            return daoSymbol;
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService]infura dao symbol erc20_token:{} exception:{}", erc20_token, e);
        }
        return null;
    }

    /**
     * 查询erc20Name
     *
     * @param erc20_token
     * @return
     */
    private String getErc20Name(String erc20_token) {
        try {
            InfuraCallRequestDto infuraCallRequestDto3 = new InfuraCallRequestDto();
            infuraCallRequestDto3.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto3.setTo(erc20_token);
            infuraCallRequestDto3.setData(ContractMethodEnum.NAME.getMethodAddress());

            // 查询erc20Name
            Result<String> resul3 = iSubscriptionService.infuraCall(infuraCallRequestDto3);
            if (resul3.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] erc20 name error result:{}", resul3.getResultDesc());
                throw new RuntimeException("保存project查询erc20name信息失败");
            }
            log.info("[CreateProjectParamEmittedFourChainService]infura erc20 name return data:{}", resul3.getData());
            String projectInfoData3 = CommonUtil.removeHexPrefixIfExists(resul3.getData());
            List<String> dataList3 = CommonUtil.splitBy32Bytes(projectInfoData3);
            String erc20Name = CommonUtil.dynamicArgumentDecoding(projectInfoData3, dataList3.get(0), true);
            return erc20Name;
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService]infura erc20 name erc20_token:{} exception:{}", erc20_token, e);
        }
        return null;
    }

    /**
     * 查询 PROJECT_PRICE
     *
     * @param projectId
     * @return
     */
    private String getProjectPrice(String projectId) {
        InfuraCallRequestDto infuraCallPrice = new InfuraCallRequestDto();
        infuraCallPrice.setNetWork(ProtoDaoConstant.netWork);
        infuraCallPrice.setTo(ContractMethodEnum.PROJECT_PRICE.getContractAddress());
        infuraCallPrice.setData(ContractMethodEnum.PROJECT_PRICE.getMethodAddress() + projectId);

        // 调用查询price
        Result<String> resultPrice = iSubscriptionService.infuraCall(infuraCallPrice);
        if (resultPrice.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[CreateProjectParamEmittedFourChainService] price error result:{}", resultPrice.getResultDesc());
            throw new RuntimeException("保存project查询price信息失败");
        }
        log.info("[CreateProjectParamEmittedFourChainService]price infura return data:{}", resultPrice.getData());
        String priceData = resultPrice.getData();

        priceData = CommonUtil.hexToTenString(priceData);
        return priceData;

    }

    private void freshProjectUri(String projectUri, NewProjectUriDto newProjectUriDto) {
        try {
            String fileName = projectUri.substring(projectUri.lastIndexOf("/") + 1);
            s3Service.deleteObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName, fileName);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);

            newProjectUriDto.setDaoStart_date(null);
            newProjectUriDto.setUser_address(null);
            newProjectUriDto.setDiscord_link(null);
            newProjectUriDto.setTwitter_link(null);
            newProjectUriDto.setOpensea_link(null);
            newProjectUriDto.setUri(null);
            newProjectUriDto.setSocialLinks(null);
            representaion.setText(JacksonUtil.obj2json(newProjectUriDto));

            s3Service.putObject(ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName,
                    representaion);
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService] freshProjectUri projectUri:{} e:", projectUri, e);
        }
    }

    private void subscribeForNewProject(Dao dao, TransactionDto transactionDto) {
        // todo异步订阅
        List<Subscribe> subscribeList = subscribeService.selectAll();
        Subscribe subscribe1 =
                subscribeList.stream().filter(v -> TradeTypeEnum.D4A_MINTNFT.getType().equals(v.getTradeType())
                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());
        Subscribe subscribex =
                subscribeList.stream().filter(v -> TradeTypeEnum.CURRENT_ROUND.getType().equals(v.getTradeType())
                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());
        Subscribe subscribe = new Subscribe();
        subscribe.setContractAddress(dao.getErc721Token());
        subscribe.setTopics(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
        subscribe.setFromBlock(transactionDto.getBlockNumber());
        subscribe.setReceiveAddress(subscribe1.getReceiveAddress());
        subscribe.setTradeType(TradeTypeEnum.TRANSFER.getType());
        subscribe.setOrderInit(subscribeList.size() + 1);
        // 每分钟监听一次
        subscribe.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

        SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
        subscribeRequestDto.setAddress(dao.getErc721Token());
        subscribeRequestDto.setFromBlock(transactionDto.getBlockNumber());
        subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
        subscribeRequestDto
                .setTopics(Collections.singletonList(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress()));
        subscribeRequestDto.setNoticeType(SubscriberTypeEnum.EVENT.getType());
        subscribeRequestDto.setNoticeUrl(subscribe1.getReceiveAddress());
        subscribeRequestDto.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
        subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
        try {
            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto);
            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
            } else {
                subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                subscribe.setFilterId(subResult.getData());
            }
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService] subscripe erc21 retry error transactionDto:{} e:{}",
                    JacksonUtil.obj2json(transactionDto), e);
            subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
        }
        log.info("subscribeForNewProject save subscribe:" + JacksonUtil.obj2json(subscribe));
        subscribeService.save(subscribe);

        // todo异步订阅
        Subscribe subscribe2 = new Subscribe();
        subscribe2.setContractAddress(dao.getErc20Token());
        subscribe2.setTopics(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
        subscribe2.setFromBlock(transactionDto.getBlockNumber());
        subscribe2.setReceiveAddress(subscribe1.getReceiveAddress());
        subscribe2.setTradeType(TradeTypeEnum.TRANSFER_ERC20.getType());
        subscribe2.setOrderInit(subscribeList.size() + 2);
        // 每分钟监听一次
        subscribe2.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

        SubscribeRequestDto subscribeRequestDto2 = new SubscribeRequestDto();
        subscribeRequestDto2.setAddress(dao.getErc20Token());
        subscribeRequestDto2.setFromBlock(transactionDto.getBlockNumber());
        subscribeRequestDto2.setNetwork(ProtoDaoConstant.netWork);
        subscribeRequestDto2
                .setTopics(Collections.singletonList(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress()));
        subscribeRequestDto2.setNoticeType(SubscriberTypeEnum.BATCH_TRAN.getType());
        subscribeRequestDto2.setNoticeUrl(subscribe1.getReceiveAddress());
        subscribeRequestDto2.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
        subscribeRequestDto2.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
        try {
            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto2);
            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] subscripe erc20 retry error transactionDto:{} resultDesc:{}",
                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
                subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
            } else {
                subscribe2.setStatus(SubscribeStatusEnum.OPEN.getType());
                subscribe2.setFilterId(subResult.getData());
            }
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService] subscripe erc20 retry error transactionDto:{} e:{}",
                    JacksonUtil.obj2json(transactionDto), e);
            subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
        }
        //  erc20地址可能重复，表中有唯一索引..会引起冲突。
        log.info("subscribeForNewProject save subscribe2:" + JacksonUtil.obj2json(subscribe2));
        Subscribe subscribeTransferERC20 = subscribeService.selectByTradeTypeAndContractTopics(TradeTypeEnum.TRANSFER_ERC20.getType(), dao.getErc20Token(), ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
        if (subscribeTransferERC20 == null) {
            subscribeService.save(subscribe2);
        }

        // todo异步订阅  getDaoCurrentRound
        Subscribe subscribe3 = new Subscribe();
        subscribe3.setContractAddress(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getContractAddress());
        subscribe3.setTopics(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress() + dao.getProjectId());
        subscribe3.setFromBlock(transactionDto.getBlockNumber());
        subscribe3.setReceiveAddress(subscribex.getReceiveAddress());
        subscribe3.setTradeType(TradeTypeEnum.getDaoCurrentRound.getType());
        subscribe3.setOrderInit(subscribeList.size() + 3);
        // 每分钟监听一次
        subscribe3.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

        SubscribeRequestDto subscribeRequestDto3 = new SubscribeRequestDto();
        subscribeRequestDto3.setAddress(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getContractAddress());
        subscribeRequestDto3.setFromBlock(transactionDto.getBlockNumber());
        subscribeRequestDto3.setNetwork(ProtoDaoConstant.netWork);
        subscribeRequestDto3
                .setTopics(Collections.singletonList(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress() + dao.getProjectId()));
        subscribeRequestDto3.setNoticeType(SubscriberTypeEnum.VALUE.getType());
        subscribeRequestDto3.setNoticeUrl(subscribex.getReceiveAddress());
        subscribeRequestDto3.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
        subscribeRequestDto3.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
        try {
            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto3);
            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] subscripe GET_DAO_CURRENT_ROUND retry error transactionDto:{} resultDesc:{}",
                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
                subscribe3.setStatus(SubscribeStatusEnum.CLOSE.getType());
            } else {
                subscribe3.setStatus(SubscribeStatusEnum.OPEN.getType());
                subscribe3.setFilterId(subResult.getData());
            }
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService] subscripe GET_DAO_CURRENT_ROUND retry error transactionDto:{} e:{}",
                    JacksonUtil.obj2json(transactionDto), e);
            subscribe3.setStatus(SubscribeStatusEnum.CLOSE.getType());
        }
        log.info("subscribeForNewProject save subscribe3:" + JacksonUtil.obj2json(subscribe3));
        subscribeService.save(subscribe3);
    }
}
