//package semios.api.service.chain;
//
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.stereotype.Service;
//import org.springframework.web.client.RestTemplate;
//import semios.api.interceptor.S3Service;
//import semios.api.model.dto.chain.DaoEthRoyaltyToken;
//import semios.api.model.dto.chain.DaoReserveRatio;
//import semios.api.model.dto.chain.DaoRoyaltyToken;
//import semios.api.model.dto.chain.DesignatedCap;
//import semios.api.model.dto.common.BucketObjectRepresentaion;
//import semios.api.model.dto.common.ProtoDaoConstant;
//import semios.api.model.dto.common.Result;
//import semios.api.model.dto.common.ResultDesc;
//import semios.api.model.dto.request.InfuraCallRequestDto;
//import semios.api.model.dto.request.SubscribeRequestDto;
//import semios.api.model.dto.response.NewProjectUriDto;
//import semios.api.model.dto.response.TransactionDto;
//import semios.api.model.entity.*;
//import semios.api.model.enums.*;
//
//import semios.api.service.feign.ISubscriptionService;
//import semios.api.utils.CommonUtil;
//import semios.api.utils.JacksonUtil;
//import semios.api.utils.ProtoDaoCommonUtil;
//import semios.api.utils.merkle.MerkleTree;
//
//import java.math.BigDecimal;
//import java.time.LocalDate;
//import java.time.format.DateTimeFormatter;
//import java.util.*;
//
///**
// * 1.3 新建dao抛出事件
// *
// * @description:
// * @author: xiangbin
// * @create: 2023-11-10
// **/
//@Deprecated
//@Slf4j
//@Service
//public class CreateProjectParamEmittedForFundingChainService implements SubscriberChainService {
//    // v2 https://goerli.etherscan.io/tx/0x001a4a459e1670e2988f1b73efe7cb40e35a7b84726c4a87bf19db9f5b5fd6ea#eventlog
//
//    @Autowired(required = false)
//    private ISubscriptionService iSubscriptionService;
//
//    @Autowired
//    private IDaoService daoService;
//
//    @Autowired
//    private ISubscribeService subscribeService;
//
//    @Autowired
//    private IDaoStrategyService daoStrategyService;
//
//    @Autowired
//    private IWhiteListService whiteListService;
//
//    @Autowired
//    private IDaoDrbStatisticsService daoDrbStatisticsService;
//
//    private static final RestTemplate REST_TEMPLATE = new RestTemplate();
//
//    @Autowired
//    private S3Service s3Service;
//
//    @Value("${dao_default_logo}")
//    private String daoDefaultLogo;
//    @Value("${dao_default_bgbanner}")
//    private String daoDefaultBgbanner;
//
//    @Override
//    public void handleTrade(TransactionDto transactionDto) throws Exception {
//
//        log.info("[CreateProjectParamEmittedForFundingChainService]transactionDto:{}", JacksonUtil.obj2json(transactionDto));
//        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//        String projectId = dataList.get(0);
//        String feePool = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
//        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
//        String erc721_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));
//
//        /**
//         * 1. 解析参数
//         * 2. 查询uri 获取NewProjectUriDto信息
//         * 3. 查询projectInfo信息
//         * 4. 查询 FEE_RECEIPT
//         * 5. 查询 DAO_SYMBOL NAME
//         * 6. 查询 NAME
//         * 7. 查询 PROJECT_PRICE
//         * 8. 构建dao信息 保存dao信息
//         * 9. 刷新uri信息
//         * 10.订阅dao相关的订阅信息
//         */
//
//        //1. 解析参数 daoMetadataParam
//        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(4)))) >> 5;
//        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(5)))) >> 5;
//        int c = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(6)))) >> 5;
//        int d = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(7)))) >> 5;
//        int e = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(8)))) >> 5;
//        //dao信息
//        String daoMetadataParam = "";
//        //dao 白名单信息
//        String whitelist = "";
//        //dao黑名单信息
//        String blacklist = "";
//        // dao铸造上限信息
//        String daoMintCapParam = "";
//        // nftMinterCapInfo
//        String nftMinterCapInfo = "";
//        for (int i = a; i < b; i++) {
//            daoMetadataParam += dataList.get(i);
//        }
//        for (int i = b; i < c; i++) {
//            whitelist += dataList.get(i);
//        }
//        for (int i = c; i < d; i++) {
//            blacklist += dataList.get(i);
//        }
//        for (int i = d; i < e; i++) {
//            daoMintCapParam += dataList.get(i);
//        }
//        for (int i = e; i < dataList.size(); i++) {
//            nftMinterCapInfo += dataList.get(i);
//        }
//
//        List<String> daoMetadataParamList = CommonUtil.splitBy32Bytes(daoMetadataParam);
//        String startDrb = CommonUtil.hexToTenString(daoMetadataParamList.get(0));
//        String mintableRounds = CommonUtil.hexToTenString(daoMetadataParamList.get(1));
//        //String floorPriceRank = CommonUtil.hexToTenString(daoMetadataParamList.get(2));
//        //String maxNftRank = CommonUtil.hexToTenString(daoMetadataParamList.get(3));
//        String royaltyFee = CommonUtil.hexToTenString(daoMetadataParamList.get(4));
//        String projectUri = CommonUtil.dynamicArgumentDecoding(daoMetadataParam, daoMetadataParamList.get(5), true);
//        //String projectIndex = CommonUtil.hexToTenString(daoMetadataParamList.get(6));
//
//        log.info("[CreateProjectParamEmittedForFundingChainService]uri:{}", projectUri);
//        Dao dao = daoService.daoDetailByUri(projectUri);
//        if (dao != null) {
//            log.info("[CreateProjectParamEmittedForFundingChainService]dao is exits uri:{}", projectUri);
//            return;
//        }
//
//        NewProjectUriDto newProjectUriDto = REST_TEMPLATE.getForObject(projectUri, NewProjectUriDto.class);
//        log.info("[CreateProjectParamEmittedForFundingChainService]uri return projectInfo:{}", JacksonUtil.obj2json(newProjectUriDto));
//        if (newProjectUriDto == null || StringUtils.isBlank(newProjectUriDto.getName())) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] error newProjectUriDto is null transactionDto:{}",
//                    JacksonUtil.obj2json(transactionDto));
//            throw new RuntimeException("newProjectUriDto is null");
//        }
//
//        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
//        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
//        infuraCallRequestDto.setTo(ContractMethodEnum.PROJECT_INFO.getContractAddress());
//        infuraCallRequestDto.setData(ContractMethodEnum.PROJECT_INFO.getMethodAddress() + projectId);
//
//        // 查询合约的projectInfo
//        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
//        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] error result:{}", result.getResultDesc());
//            throw new RuntimeException("保存project查询信息失败");
//        }
//        log.info("[CreateProjectParamEmittedForFundingChainService]infura PROJECT_INFO return data:{}", result.getData());
//        String projectInfoData = result.getData();
//        List<String> projectDataList = CommonUtil.splitBy32Bytes(projectInfoData);
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
//        // newProjectUriDto.setName("d4a-"+index + "-" + dao.getDaoName().toLowerCase().replaceAll(" ", "-"));
//        if (StringUtils.isNotBlank(royaltyFee)) {
//            newProjectUriDto.setSeller_fee_basis_points(Integer.valueOf(royaltyFee));
//        }
//        //获取fee_recipient地址
//        String fee_recipient = this.getFeeRecipient(projectId);
//        newProjectUriDto.setFee_recipient(CommonUtil.addHexPrefixIfNotExist(fee_recipient));
//        log.info("[CreateProjectParamEmittedForFundingChainService]uri return projectInfo:{}", JacksonUtil.obj2json(newProjectUriDto));
//        //通过合约查询daoSymbol信息
//        String daoSymbol = this.getDaoSymbol(erc20_token);
//
//        //通过合约查询erc20Name信息
//        String erc20Name = this.getErc20Name(erc20_token);
//
//        // 通过合约查询project price
//        String priceData = getProjectPrice(projectId);
//
//        //查询input参数看是否自动生成work
//        // 获取transactionHash的参数input，从中获取调用方法的参数canvasId和projectId
//        result = iSubscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
//                CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
//        log.info("[CreateProjectParamEmittedForFundingChainService] result:{}", result.getData());
//        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] error result:{}", result.getResultDesc());
//            return;
//        }
//        Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
//        if (objectMap == null) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] objectMap is null result:{}", result.getData());
//            return;
//        }
//        String input = (String) objectMap.get("input");
//        input = input.substring(10);
//        input = input.substring(0, 19 * 64);
//        List<String> inputDataList = CommonUtil.splitBy32Bytes(input);
//        String addWork = CommonUtil.hexToTenString(inputDataList.get(inputDataList.size() - 2));
//
//        dao = new Dao();
//        dao.setDaoSymbol(daoSymbol);
//        dao.setErc20Name(erc20Name);
//        if (StringUtils.isNotBlank(index)) {
//            dao.setDaoNumber(Integer.valueOf(index));
//        }
//        dao.setDaoStatus(DaoStatusEnum.NOT_STARTED.getStatus());
//        if (StringUtils.isNotBlank(startDrb)
//                && Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).compareTo(Integer.valueOf(startDrb)) >= 0) {
//            dao.setDaoStatus(DaoStatusEnum.STARTED.getStatus());
//        }
////        dao.setDaoStatus(-1);
//        dao.setSyncDex(1);
//        dao.setBlockTime(transactionDto.getBlockTime());
//        dao.setTransactionHash(transactionDto.getTransactionHash());
//        dao.setBlockNumber(transactionDto.getBlockNumber());
//        dao.setProjectId(projectId);
//        dao.setReceivedToken(BigDecimal.ZERO);
//        dao.setUnclaimedToken(BigDecimal.ZERO);
////        dao.setFeePool(feePool);
//        //feepool指的是redeem那个池子
//        dao.setDaoRedeemPool(feePool);
//        dao.setErc20Token(erc20_token);
//        dao.setErc721Token(erc721_token);
//        dao.setRoyaltyFee(royaltyFee);
//        dao.setBasicDao(BasicDaoEnum.PROTO_DAO.getBasicType());
//        // 1.8.5之后的都可以修改
//        dao.setDaoVersion(3);
//        if (StringUtils.isNotBlank(erc20_total_supply)) {
//            dao.setErc20TotalSupply(new BigDecimal(erc20_total_supply).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)).toPlainString());
//        }
//
//        dao.setDaoName(newProjectUriDto.getName().replaceFirst("ProtoDao ", ""));
//        dao.setDaoManitesto(newProjectUriDto.getManitesto());
//        dao.setDaoDescription(newProjectUriDto.getDescription());
//        dao.setSocialLinks(newProjectUriDto.getSocialLinks());
//        dao.setWorkUrlSuffix(".png");
//        LocalDate dateParam;
//        try {
//            DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
//            dateParam = LocalDate.parse(newProjectUriDto.getDaoStart_date(), df);
//        } catch (Exception ex) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] error dateParam is null daoStartDate:{}",
//                    newProjectUriDto.getDaoStart_date());
//            dateParam = null;
//        }
//
//        dao.setDaoStartDate(dateParam);
//        dao.setAddWork("0".equals(addWork) ? 1 : 0);
//        dao.setNeedMintableWork("0".equals(addWork) ? 1 : 0);
//
//        if (StringUtils.isNotBlank(startDrb)) {
//            dao.setDaoStartDrb(Integer.valueOf(startDrb));
//        }
//        if (StringUtils.isNotBlank(max_nft_amount)) {
//            dao.setTotalNftCasting(Integer.valueOf(max_nft_amount));
//        }
//        if (StringUtils.isNotBlank(mintableRounds)) {
//            dao.setDaoMintWindow(Integer.valueOf(mintableRounds));
//        }
//
//
//        if (StringUtils.isNotBlank(priceData)) {
//            dao.setDaoFloorPrice(new BigDecimal(priceData).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//            dao.setCanvasFloorPrice(new BigDecimal(priceData).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//        }
//
//        if (StringUtils.isNotBlank(royaltyFee)) {
//            DaoCreateFeeEnum daoCreateFeeEnum = DaoCreateFeeEnum
//                    .getDaoCreateFeeEnumByActualValue(Integer.parseInt(royaltyFee) - ProtoDaoConstant.MINT_D4A_FEE_RATIO);
//            if (daoCreateFeeEnum != null) {
//                dao.setDaoCreateFee(Float.parseFloat(String.valueOf(daoCreateFeeEnum.getShowValue())));
//            }
//        }
//
//        dao.setOpenseaLink(newProjectUriDto.getOpensea_link());
//        dao.setTwitterLink(newProjectUriDto.getTwitter_link());
//        dao.setDiscordLink(newProjectUriDto.getDiscord_link());
//        dao.setOwnerAddress(newProjectUriDto.getUser_address());
//
//        dao.setDaoUri(newProjectUriDto.getUri());
//        if (StringUtils.isNotBlank(newProjectUriDto.getLogo())) {
//            dao.setDaoLogoUrl(newProjectUriDto.getLogo());
//        } else {
//            dao.setDaoLogoUrl(daoDefaultLogo);
//        }
//        if (StringUtils.isNotBlank(newProjectUriDto.getBg_banner())) {
//            dao.setDaoBgBanner(newProjectUriDto.getBg_banner());
//        } else {
//            dao.setDaoBgBanner(daoDefaultBgbanner);
//        }
//        dao.setDrbNumber(Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND));
//
//
//        //erc20 发放方式、价格变动及相关参数设置  templateParam
//        buildTemplateParam(transactionDto, dao);
//
//
//        List<DaoStrategy> daoStrategyList = new ArrayList<>();
//        //更新白名单信息 这里不处理，在WhitelistModifiedChainService服务里处理，创建dao的时候同时会触发这个事件
////        List<DaoStrategy> daoWhiteListStrategyList = buildWhitelist(transactionDto, whitelist, dao);
////        if (daoWhiteListStrategyList != null && daoWhiteListStrategyList.size() > 0) {
////            daoStrategyList.addAll(daoWhiteListStrategyList);
////        }
//
//        //更新黑名单信息 blacklist
//        List<DaoStrategy> daoBlackListStrategyList = buildBlacklist(transactionDto, blacklist, dao);
//        if (daoBlackListStrategyList != null && daoBlackListStrategyList.size() > 0) {
//            daoStrategyList.addAll(daoBlackListStrategyList);
//        }
//        //更新铸造上限 daoMintCapParam
//        List<DaoStrategy> daoMintCapStrategyList = buildDaoMintCapParam(transactionDto, daoMintCapParam, dao);
//
//        if (daoMintCapStrategyList != null && daoMintCapStrategyList.size() > 0) {
//            daoStrategyList.addAll(daoMintCapStrategyList);
//        }
//
//        //nftMinterCapInfo
//        List<DaoStrategy> nftMinterCapInfoStrategyList = buildNftMinterCapInfo(transactionDto, nftMinterCapInfo, dao);
//
//        if (nftMinterCapInfoStrategyList != null && nftMinterCapInfoStrategyList.size() > 0) {
//            daoStrategyList.addAll(nftMinterCapInfoStrategyList);
//        }
//        //basicDaoParam 信息先不处理
//        //actionType 它是合约调方法的一个判断符号，现在所有的创建应该都是默认20
//        // String actionType = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
//
//        //更新dao收益分配 allRatioForFundingParam
//        buildAllRatioForFundingParam(transactionDto, dao);
//
//        //存储dao信息
//        daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
//
//        //0914 创建dao的drb，如果没有铸造，会少计算一个drb的问题
//        if (dao.getRoyaltyTokenLotteryMode() != null && dao.getRoyaltyTokenLotteryMode() == 1) {
//            DaoDrbStatistics daoDrbStatistics1 = new DaoDrbStatistics();
//            daoDrbStatistics1.setFloorPrice(dao.getDaoFloorPrice());
//            daoDrbStatistics1.setDaoId(dao.getId());
//            daoDrbStatistics1.setStatus(StatisticsStatusEnum.WJS.getStatus());
//            daoDrbStatistics1.setDrbVol(BigDecimal.ZERO);
//            daoDrbStatistics1.setDrbVolExTax(BigDecimal.ZERO);
//
//            daoDrbStatistics1.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
//            log.info("[CreateProjectParamEmittedForFundingChainService] daoId:{} start add DaoDrbStatistics daoStatus:{}", dao.getId(), dao.getDaoStatus());
//            daoDrbStatisticsService.save(daoDrbStatistics1);
//        }
//
//        // 刷新uri
//        this.freshProjectUri(projectUri, newProjectUriDto);
//
//        // 订阅
//        this.subscribeForNewProject(dao, transactionDto);
//
//
//    }
//
//    /**
//     * erc20 发放方式、价格变动及相关参数设置
//     *
//     * @param transactionDto
//     * @param dao
//     */
//    private void buildTemplateParam(TransactionDto transactionDto, Dao dao) {
//        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//        //指数变价传 0，线性变价传 1
//        String priceTemplateType = CommonUtil.hexToTenString(dataList.get(9));
//        //变价的系数，如果选择了指数变价，则 priceFactor 传翻倍的系数  如果选择了线性变价，则传价格变动时，每次增加或者减少的价格数量，
//        String priceFactor = CommonUtil.hexToTenString(dataList.get(10));
//        //ERC20 奖励发放类型 线性发放奖励传 0，奖励发放指数衰减传 1  该参数应该传2（枚举类型）
//        String rewardTemplateType = CommonUtil.hexToTenString(dataList.get(11));
//        //奖励发放衰减的系数  例如均匀发放，则传 0,每次减少发放 10000 个 token，则传 10000 * 1e18  // 该参数已废弃，随便传参数即可
//        String rewardDecayFactor = CommonUtil.hexToTenString(dataList.get(12));
//        //衰减周期长度，默认传 1，表示一个周期中有 1 个 DRB
////        String rewardDecayLife = CommonUtil.hexToTenString(dataList.get(17));
//        //是否累计奖池（乐透模式），是则传 true，不是传 false
//        String isProgressiveJackpot = CommonUtil.hexToTenString(dataList.get(13));
//
//        dao.setCanvasPriceFluctuationMethod(Integer.valueOf(priceTemplateType));
//        dao.setFluctuationMethodFactor(new BigDecimal(priceFactor));
//        dao.setRoyaltyTokenGenerationMethod(Integer.valueOf(rewardTemplateType));
//        dao.setRoyaltyTokenGenerationFactory(new BigDecimal(rewardDecayFactor));
//        dao.setRoyaltyTokenLotteryMode(Integer.valueOf(isProgressiveJackpot));
//
////        daoService.updateById(dao);
//    }
//
//    /**
//     * 更新dao收益分配 allRatioForFundingParam
//     *
//     * @param transactionDto
//     * @param dao
//     */
//    private void buildAllRatioForFundingParam(TransactionDto transactionDto, Dao dao) {
//
//        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//        //DAO Price Reserve Ratio
//        String canvasCreatorMintFeeRatio = CommonUtil.hexToTenString(dataList.get(16));// Builder Mint Fee
//        String assetPoolMintFeeRatio = CommonUtil.hexToTenString(dataList.get(17));// SubDAO Mint Fee
//        String redeemPoolMintFeeRatio = CommonUtil.hexToTenString(dataList.get(18));// MainDAO Mint Fee
//        // 以上三个参数加 PAO Mint Fee 等于 10000
//
//
//        // Fixed Price Reserve Ratio
//        String canvasCreatorMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(19));// Builder Mint Fee
//        String assetPoolMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(20));// SubDAO Mint Fee
//        String redeemPoolMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(21));// MainDAO Mint Fee
//        // 以上三个参数加 PDAO Mint Fee 等于 10000
//
//        //Royalty Token
//        String minterERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(22));// Minter Reward
//        String canvasCreatorERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(23));// Builder Reward
//        String daoCreatorERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(24));// SubDAO Starter Reward
//        // 以上三个参数加 PDAO Reward 等于 10000
//
//        // ETH
//        String minterETHRewardRatio = CommonUtil.hexToTenString(dataList.get(25));// Minter Reward
//        String canvasCreatorETHRewardRatio = CommonUtil.hexToTenString(dataList.get(26));// Builder Reward
//        String daoCreatorETHRewardRatio = CommonUtil.hexToTenString(dataList.get(27));// SubDAO Starter Reward
//        // 以上三个参数加 PDAO Reward 等于 10000
//
//        //DAO Price Reserve Ratio
//        DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
//        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
//            daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
//        }
//        daoRoyaltyToken.setDaoReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorERC20RewardRatio));
//        daoRoyaltyToken.setCanvasReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorERC20RewardRatio));
//        daoRoyaltyToken.setMinterReward(ProtoDaoCommonUtil.strToBigDecimal(minterERC20RewardRatio));
//        daoRoyaltyToken.setD4aReward(new BigDecimal("100").subtract(daoRoyaltyToken.getDaoReward()).subtract(daoRoyaltyToken.getCanvasReward()).subtract(daoRoyaltyToken.getMinterReward()));
//
//        //ETH
//        DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
//        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
//            daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);
//        }
//        daoEthRoyaltyToken.setMinterETHReward(ProtoDaoCommonUtil.strToBigDecimal(minterETHRewardRatio));
//        daoEthRoyaltyToken.setCanvasCreatorETHReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorETHRewardRatio));
//        daoEthRoyaltyToken.setDaoCreatorETHReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorETHRewardRatio));
//        daoEthRoyaltyToken.setD4aReward(new BigDecimal("100").subtract(daoEthRoyaltyToken.getMinterETHReward()).subtract(daoEthRoyaltyToken.getCanvasCreatorETHReward()).subtract(daoEthRoyaltyToken.getDaoCreatorETHReward()));
//
//        // 非一口价
//        DaoReserveRatio daoReserveRatio = new DaoReserveRatio(true);
//        if (StringUtils.isNotBlank(dao.getFixedReserveRatio())) {
//            daoReserveRatio = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
//        }
//        daoReserveRatio.setCanvasMintFee(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorMintFeeRatio));
//        daoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(assetPoolMintFeeRatio));
//        daoReserveRatio.setRedeemPoolMintFee(ProtoDaoCommonUtil.strToBigDecimal(redeemPoolMintFeeRatio));
//        daoReserveRatio.setD4aMintFee(new BigDecimal("100").subtract(daoReserveRatio.getDaoMintFee()).subtract(daoReserveRatio.getCanvasMintFee()).subtract(daoReserveRatio.getRedeemPoolMintFee()));
//        // 一口价
//        DaoReserveRatio fixedDaoReserveRatio = new DaoReserveRatio(false);
//        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
//            fixedDaoReserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
//        }
//        fixedDaoReserveRatio.setCanvasMintFee(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorMintFeeRatioFiatPrice));
//        fixedDaoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(assetPoolMintFeeRatioFiatPrice));
//        fixedDaoReserveRatio.setRedeemPoolMintFee(ProtoDaoCommonUtil.strToBigDecimal(redeemPoolMintFeeRatioFiatPrice));
//        fixedDaoReserveRatio.setD4aMintFee(new BigDecimal("100").subtract(fixedDaoReserveRatio.getDaoMintFee()).subtract(fixedDaoReserveRatio.getCanvasMintFee()).subtract(fixedDaoReserveRatio.getRedeemPoolMintFee()));
//
//
//        dao.setRoyaltyToken(JacksonUtil.obj2json(daoRoyaltyToken));
//        dao.setEthRoyaltyToken(JacksonUtil.obj2json(daoEthRoyaltyToken));
//        dao.setUnfixedReserveRatio(JacksonUtil.obj2json(daoReserveRatio));
//        dao.setFixedReserveRatio(JacksonUtil.obj2json(fixedDaoReserveRatio));
//
//        log.info("[DaoRatioSetChainService] daoId:{} royaltyToken:{}", dao.getId(), dao.getRoyaltyToken());
//        log.info("[DaoRatioSetChainService] daoId:{} unfixedReserveRatio:{}", dao.getId(),
//                dao.getUnfixedReserveRatio());
//        log.info("[DaoRatioSetChainService] daoId:{} fixedReserveRatio:{}", dao.getId(), dao.getFixedReserveRatio());
////        daoService.updateById(dao);
//    }
//
//    /**
//     * 更新铸造上限
//     *
//     * @param transactionDto
//     * @param daoMintCapParam
//     * @param dao
//     */
//    private List<DaoStrategy> buildDaoMintCapParam(TransactionDto transactionDto, String daoMintCapParam, Dao dao) {
//
//        String data = CommonUtil.removeHexPrefixIfExists(daoMintCapParam);
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//        String mintCap = CommonUtil.hexToTenString(dataList.get(0));
//
//        String capSize = CommonUtil.hexToTenString(dataList.get(2));
//        List<DesignatedCap> designatedCaps = null;
//        if (StringUtils.isNotBlank(capSize) && Integer.parseInt(capSize) > 0) {
//            designatedCaps = new ArrayList<>(Integer.parseInt(capSize));
//            for (int i = 0; i < Integer.parseInt(capSize); i++) {
//                DesignatedCap designatedCap = new DesignatedCap();
//                String account = CommonUtil
//                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 3))).toLowerCase();
//                designatedCap.setAccount(account);
//
//                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 4));
//                if (StringUtils.isNotBlank(cap)) {
//                    designatedCap.setCap(Integer.parseInt(cap));
//                    if (Integer.parseInt(cap) > 0) {
//                        designatedCaps.add(designatedCap);
//                    }
//                }
//            }
//        }
//        List<DaoStrategy> daoStrategyList = new ArrayList<>();
//        DaoStrategy newDaoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
//                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
//        if (newDaoStrategy == null) {
//            newDaoStrategy = new DaoStrategy();
//            newDaoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
//            newDaoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY.getType());
//            newDaoStrategy.setProofId(dao.getId());
//            newDaoStrategy.setDaoUri(dao.getDaoUri());
//            newDaoStrategy.setDaoId(dao.getId());
//            newDaoStrategy.setProjectId(dao.getProjectId());
//            newDaoStrategy.setDaoNumber(dao.getDaoNumber());
//        } else {
//            log.info("[MintCapAddedChainService] daoId:{} origin daoStrategy:{}", dao.getId(),
//                    JacksonUtil.obj2json(newDaoStrategy));
//        }
//
//        newDaoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//        newDaoStrategy.setBlockTime(transactionDto.getBlockTime());
//
//        if (designatedCaps != null && !designatedCaps.isEmpty()) {
//            newDaoStrategy.setOriginAddress(JacksonUtil.obj2json(designatedCaps));
//            if (dao.getMintCap() == null || dao.getMintCap() == 0) {
//                dao.setMintCap(1);
//            }
//        } else {
//            newDaoStrategy.setOriginAddress(null);
//            if (dao.getMintCap() != null && dao.getMintCap() == 1) {
//                dao.setMintCap(0);
//            }
//        }
//
//        daoStrategyList.add(newDaoStrategy);
//
//        if (StringUtils.isNotBlank(mintCap)) {
//            dao.setGlobalMintCap(Integer.parseInt(mintCap));
//        }
//
//        log.info("[MintCapAddedChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
//                JacksonUtil.obj2json(daoStrategyList));
//        return daoStrategyList;
////        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
////        log.info("[MintCapAddedChainService] daoId:{} return i:{}", dao.getId(), i);
//    }
//
//
//    /**
//     * 更新铸造上限
//     *
//     * @param transactionDto
//     * @param nftMinterCapInfo
//     * @param dao
//     */
//    private List<DaoStrategy> buildNftMinterCapInfo(TransactionDto transactionDto, String nftMinterCapInfo, Dao dao) {
//
//        String data = CommonUtil.removeHexPrefixIfExists(nftMinterCapInfo);
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//        String nftMinterCapSize = CommonUtil.hexToTenString(dataList.get(0));
//        Integer nftMinterCapCount = Integer.parseInt(nftMinterCapSize);
//        List<DesignatedCap> nftDesignatedCaps = null;
//        if (StringUtils.isNotBlank(nftMinterCapSize) && Integer.parseInt(nftMinterCapSize) > 0) {
//            nftDesignatedCaps = new ArrayList<>();
//            for (int i = 0; i < nftMinterCapCount; i++) {
//                DesignatedCap designatedCap = new DesignatedCap();
//                String account = CommonUtil
//                        .addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2 * i + 1))).toLowerCase();
//                designatedCap.setAccount(account);
//
//                String cap = CommonUtil.hexToTenString(dataList.get(2 * i + 2));
//                if (StringUtils.isNotBlank(cap)) {
//                    designatedCap.setCap(Integer.parseInt(cap));
//                    if (Integer.parseInt(cap) > 0) {
//                        nftDesignatedCaps.add(designatedCap);
//                    }
//                }
//            }
//        }
//
//        DaoStrategy nftMinterStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
//                DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
//        if (nftMinterStrategy == null) {
//            nftMinterStrategy = new DaoStrategy();
//            nftMinterStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
//            nftMinterStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.HIGH_PRIORITY_ERC721.getType());
//            nftMinterStrategy.setProofId(dao.getId());
//            nftMinterStrategy.setDaoUri(dao.getDaoUri());
//            nftMinterStrategy.setDaoId(dao.getId());
//            nftMinterStrategy.setProjectId(dao.getProjectId());
//            nftMinterStrategy.setDaoNumber(dao.getDaoNumber());
//        } else {
//            log.info("[buildNftMinterCapInfo] daoId:{} HIGH_PRIORITY_ERC721 origin daoStrategy:{}", dao.getId(),
//                    JacksonUtil.obj2json(nftMinterStrategy));
//        }
//
//        nftMinterStrategy.setTransactionHash(transactionDto.getTransactionHash());
//        nftMinterStrategy.setBlockTime(transactionDto.getBlockTime());
//
//        if (nftDesignatedCaps != null && !nftDesignatedCaps.isEmpty()) {
//            nftMinterStrategy.setOriginAddress(JacksonUtil.obj2json(nftDesignatedCaps));
//            if (dao.getErc721MintCap() == null || dao.getErc721MintCap() == 0) {
//                dao.setId(dao.getId());
//                dao.setErc721MintCap(1);
//            }
//        } else {
//            nftMinterStrategy.setOriginAddress(null);
//            if (dao.getErc721MintCap() != null && dao.getErc721MintCap() == 1) {
//                dao.setId(dao.getId());
//                dao.setErc721MintCap(0);
//            }
//        }
//
//        List<DaoStrategy> daoStrategyList = new ArrayList<>();
//        daoStrategyList.add(nftMinterStrategy);
//
//        log.info("[buildNftMinterCapInfo] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
//                JacksonUtil.obj2json(daoStrategyList));
//        return daoStrategyList;
//    }
//
//    /**
//     * 更新黑名单信息
//     *
//     * @param transactionDto
//     * @param blacklist
//     * @param dao
//     */
//    private List<DaoStrategy> buildBlacklist(TransactionDto transactionDto, String blacklist, Dao dao) {
//
//        List<String> dataList = CommonUtil.splitBy32Bytes(blacklist);
//
//        List<String> minterNftHolderPass = new ArrayList<>();
//        List<String> canvasCreatorNftHolderPass = new ArrayList<>();
//        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(0)))) >> 5;
//        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(1)))) >> 5;
//        for (int i = a + 1; i < b; i++) {
//            String mintNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
//            if (ProtoDaoConstant.ZERO_ADDRESS.equals(mintNftHolder)) {
//                continue;
//            }
//            minterNftHolderPass.add(mintNftHolder);
//        }
//
//
//        for (int i = b + 1; i < dataList.size(); i++) {
//            String canvasCreatorNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
//            if (ProtoDaoConstant.ZERO_ADDRESS.equals(canvasCreatorNftHolder)) {
//                continue;
//            }
//            canvasCreatorNftHolderPass.add(canvasCreatorNftHolder);
//        }
//
//        if (minterNftHolderPass.isEmpty() && canvasCreatorNftHolderPass.isEmpty()) {
//            return null;
//        }
//
//        List<DaoStrategy> daoStrategyList = new ArrayList<>();
//        if (!minterNftHolderPass.isEmpty()) {
//            DaoStrategy daoStrategy = new DaoStrategy();
//            daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
//            daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
//            daoStrategy.setOriginAddress(String.join(",", minterNftHolderPass));
//            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//            daoStrategy.setBlockTime(transactionDto.getBlockTime());
//            daoStrategy.setDaoUri(dao.getDaoUri());
//            daoStrategy.setDaoId(dao.getId());
//            daoStrategy.setProjectId(dao.getProjectId());
//            daoStrategy.setDaoNumber(dao.getDaoNumber());
//            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//            daoStrategy.setBlockTime(transactionDto.getBlockTime());
//            daoStrategyList.add(daoStrategy);
//
//            dao.setMinterWorksBlacklist(DaoBlackListEnum.OPEN.getStatus());
//        }
//
//
//        if (!canvasCreatorNftHolderPass.isEmpty()) {
//
//
//            DaoStrategy daoStrategy = new DaoStrategy();
//            daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
//            daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.BLACK_LIST.getType());
//            daoStrategy.setOriginAddress(String.join(",", canvasCreatorNftHolderPass));
//            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//            daoStrategy.setBlockTime(transactionDto.getBlockTime());
//            daoStrategy.setDaoUri(dao.getDaoUri());
//            daoStrategy.setDaoId(dao.getId());
//            daoStrategy.setProjectId(dao.getProjectId());
//            daoStrategy.setDaoNumber(dao.getDaoNumber());
//            daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//            daoStrategy.setBlockTime(transactionDto.getBlockTime());
//
//            daoStrategyList.add(daoStrategy);
//
//            dao.setCanvasCreatedBlacklist(DaoBlackListEnum.OPEN.getStatus());
//
//        }
//
//        return daoStrategyList;
////        daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
//    }
//
//
//    /**
//     * 更新白名单信息
//     *
//     * @param transactionDto
//     * @param whitelist
//     * @param dao
//     */
//    private List<DaoStrategy> buildWhitelist(TransactionDto transactionDto, String whitelist, Dao dao) {
//        try {
//            List<String> dataList = CommonUtil.splitBy32Bytes(whitelist);
//            String minterMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
//            String canvasCreatorMerkleRoot = CommonUtil.addHexPrefixIfNotExist(dataList.get(2));
//            WhiteList whiteList = null;
//
//            List<String> minterNftHolderPass = new ArrayList<>();
//            List<String> canvasCreatorNftHolderPass = new ArrayList<>();
//            int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(1)))) >> 5;
//            int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(3)))) >> 5;
//            for (int i = a; i < b; i++) {
//                String mintNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
//                if (ProtoDaoConstant.ZERO_ADDRESS.equals(mintNftHolder)) {
//                    continue;
//                }
//                minterNftHolderPass.add(mintNftHolder);
//            }
//
//
//            for (int i = b; i < dataList.size(); i++) {
//                String canvasCreatorNftHolder = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(i))).toLowerCase();
//                if (ProtoDaoConstant.ZERO_ADDRESS.equals(canvasCreatorNftHolder)) {
//                    continue;
//                }
//                canvasCreatorNftHolderPass.add(canvasCreatorNftHolder);
//            }
//
//
//            List<DaoStrategy> daoStrategyList = new ArrayList<>();
//
//            log.info(
//                    "[CreateProjectParamEmittedForFundingChainService] daoId:{} minterMerkleRoot:{} minterNftHolderPass:{} canvasCreatorMerkleRoot:{} canvasCreatorNftHolderPass:{}",
//                    dao.getId(), minterMerkleRoot, JacksonUtil.obj2json(minterNftHolderPass), canvasCreatorMerkleRoot,
//                    JacksonUtil.obj2json(canvasCreatorNftHolderPass));
//            if (StringUtils.isNotBlank(minterMerkleRoot)) {
//                if (minterMerkleRoot.equals(ProtoDaoConstant.ZERO_MERKLE_ROOT)) {
//                    if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getMinterWorksWhitelist())
//                            || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getMinterWorksWhitelist())) {
//                        dao.setMinterWorksWhitelist(dao.getMinterWorksWhitelist() - 1);
//                        // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
//                    }
//                } else {
//                    // 查询merkleRoot
//                    whiteList = whiteListService.selectByAddressAndRoot(dao.getOwnerAddress(), minterMerkleRoot);
//                    if (whiteList == null) {
//                        whiteList = new WhiteList();
//                        whiteList.setUserAddress(dao.getOwnerAddress());
//                        String originAddress = dao.getOwnerAddress();
//                        List<String> list = Arrays.asList(originAddress);
//                        MerkleTree mt = new MerkleTree(list);
//                        mt.init();
//                        String rootHash = mt.getRootHash();
//                        if (!rootHash.equals(minterMerkleRoot.toLowerCase())) {
//                            throw new RuntimeException(
//                                    "diff root hash, front:" + minterMerkleRoot + ", end:" + rootHash);
//                        }
//                        // mintingWhiteList.setOriginAddress(daoWhiteListReqVo.getMintingOriginAddress());
//                        whiteList.setOriginAddress(JacksonUtil.obj2json(list));
//                        whiteList.setProof(JacksonUtil.obj2json(mt));
//                        whiteList.setProofRootHash(minterMerkleRoot);
//                        whiteListService.save(whiteList);
////                    throw new RuntimeException("CreateProjectParamEmittedForFundingChainService cannot find whiteList");
//                    }
//
//                    DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
//                            DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
//                    if (daoStrategy == null) {
//                        daoStrategy = new DaoStrategy();
//                        daoStrategy.setProofId(whiteList.getId());
//                        daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
//                        daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
//                        daoStrategy.setOriginAddress(whiteList.getOriginAddress()
//                                .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
//                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                        daoStrategy.setDaoUri(dao.getDaoUri());
//                        daoStrategy.setDaoId(dao.getId());
//                        daoStrategy.setProjectId(dao.getProjectId());
//                        daoStrategy.setDaoNumber(dao.getDaoNumber());
//
//                    } else {
//                        daoStrategy.setOriginAddress(whiteList.getOriginAddress()
//                                .substring(1, whiteList.getOriginAddress().length() - 1).replaceAll("\"", ""));
//                        daoStrategy.setProofId(whiteList.getId());
//                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                    }
//
//                    if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getMinterWorksWhitelist())) {
//                        dao.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS.getStatus());
//                    }
//
//                    if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getMinterWorksWhitelist())) {
//                        dao.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
//                    }
//
//                    daoStrategyList.add(daoStrategy);
//                    // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
//                }
//            }
//
//            if (minterNftHolderPass.isEmpty()) {
//                // if (minterNftHolderPass.get(0).equals(ProtoDaoConstant.ZERO_ADDRESS)) {
//                if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getMinterWorksWhitelist())
//                        || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getMinterWorksWhitelist())) {
//                    dao.setMinterWorksWhitelist(dao.getMinterWorksWhitelist() - 2);
//                    // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
//                }
//            } else {
//                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
//                        DaoStrategyTypeEnum.MINT_WORK.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
//                if (daoStrategy == null) {
//                    daoStrategy = new DaoStrategy();
//                    daoStrategy.setType(DaoStrategyTypeEnum.MINT_WORK.getType());
//                    daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721.getType());
//                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNftHolderPass));
//                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                    daoStrategy.setDaoUri(dao.getDaoUri());
//                    daoStrategy.setDaoId(dao.getId());
//                    daoStrategy.setProjectId(dao.getProjectId());
//                    daoStrategy.setDaoNumber(dao.getDaoNumber());
//
//                } else {
//                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(minterNftHolderPass));
//                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                }
//
//                if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getMinterWorksWhitelist())) {
//                    dao.setMinterWorksWhitelist(DaoWhiteListEnum.ERC721.getStatus());
//                }
//
//                if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getMinterWorksWhitelist())) {
//                    dao.setMinterWorksWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
//                }
//
//                daoStrategyList.add(daoStrategy);
//                // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
//            }
//
//            if (StringUtils.isNotBlank(canvasCreatorMerkleRoot)) {
//                if (canvasCreatorMerkleRoot.equals(ProtoDaoConstant.ZERO_MERKLE_ROOT)) {
//                    if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getCanvasCreatedWhitelist())
//                            || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
//                        dao.setCanvasCreatedWhitelist(dao.getCanvasCreatedWhitelist() - 1);
//                        // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
//                    }
//                } else {
//                    // 查询merkleRoot
//                    WhiteList whiteListCanvas =
//                            whiteListService.selectByAddressAndRoot(dao.getOwnerAddress(), canvasCreatorMerkleRoot);
//                    if (whiteListCanvas == null && whiteList != null) {
//                        whiteListCanvas = whiteList;
////                        throw new RuntimeException("CreateProjectParamEmittedForFundingChainService cannot find whiteList");
//                    }
//
//                    DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
//                            DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
//                    if (daoStrategy == null) {
//                        daoStrategy = new DaoStrategy();
//                        daoStrategy.setProofId(whiteListCanvas.getId());
//                        daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
//                        daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.WHITE_LIST.getType());
//                        daoStrategy.setOriginAddress(whiteListCanvas.getOriginAddress()
//                                .substring(1, whiteListCanvas.getOriginAddress().length() - 1).replaceAll("\"", ""));
//                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                        daoStrategy.setDaoUri(dao.getDaoUri());
//                        daoStrategy.setDaoId(dao.getId());
//                        daoStrategy.setProjectId(dao.getProjectId());
//                        daoStrategy.setDaoNumber(dao.getDaoNumber());
//
//                    } else {
//                        daoStrategy.setProofId(whiteListCanvas.getId());
//                        daoStrategy.setOriginAddress(whiteListCanvas.getOriginAddress()
//                                .substring(1, whiteListCanvas.getOriginAddress().length() - 1).replaceAll("\"", ""));
//                        daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                        daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                    }
//
//                    if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
//                        dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS.getStatus());
//                    }
//
//                    if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
//                        dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
//                    }
//
//                    daoStrategyList.add(daoStrategy);
//                    // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
//                }
//            }
//
//            if (canvasCreatorNftHolderPass.isEmpty()) {
//                // if (canvasCreatorNftHolderPass.equals(ProtoDaoConstant.ZERO_ADDRESS)) {
//                if (DaoWhiteListEnum.ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())
//                        || DaoWhiteListEnum.ADDRESS_ERC721.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
//                    dao.setCanvasCreatedWhitelist(dao.getCanvasCreatedWhitelist() - 2);
//                    // daoStrategyService.saveDaoStrategyOrUpdateDao(null, dao);
//                }
//            } else {
//                DaoStrategy daoStrategy = daoStrategyService.selectDaoStrategyByType(dao.getId(),
//                        DaoStrategyTypeEnum.CREATE_CANVAS.getType(), DaoStrategyStrategyTypeEnum.ERC721.getType());
//                if (daoStrategy == null) {
//                    daoStrategy = new DaoStrategy();
//                    daoStrategy.setType(DaoStrategyTypeEnum.CREATE_CANVAS.getType());
//                    daoStrategy.setStrategyType(DaoStrategyStrategyTypeEnum.ERC721.getType());
//                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNftHolderPass));
//                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                    daoStrategy.setDaoUri(dao.getDaoUri());
//                    daoStrategy.setDaoId(dao.getId());
//                    daoStrategy.setProjectId(dao.getProjectId());
//                    daoStrategy.setDaoNumber(dao.getDaoNumber());
//
//                } else {
//                    daoStrategy.setOriginAddress(JacksonUtil.obj2json(canvasCreatorNftHolderPass));
//                    daoStrategy.setTransactionHash(transactionDto.getTransactionHash());
//                    daoStrategy.setBlockTime(transactionDto.getBlockTime());
//                }
//
//                if (DaoWhiteListEnum.CLOSE.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
//                    dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ERC721.getStatus());
//                }
//
//                if (DaoWhiteListEnum.ADDRESS.getStatus().equals(dao.getCanvasCreatedWhitelist())) {
//                    dao.setCanvasCreatedWhitelist(DaoWhiteListEnum.ADDRESS_ERC721.getStatus());
//                }
//
//                daoStrategyList.add(daoStrategy);
//                // daoStrategyService.saveDaoStrategyOrUpdateDao(daoStrategy, dao);
//            }
//
//            log.info("[CreateProjectParamEmittedForFundingChainService] dao:{} daoStrategyList:{}", JacksonUtil.obj2json(dao),
//                    JacksonUtil.obj2json(daoStrategyList));
//            dao.setDaoManitesto(dao.getDaoManitesto());
//            dao.setDaoDescription(dao.getDaoDescription());
//            dao.setFavoriteAmount(dao.getFavoriteAmount());
//
//
//            return daoStrategyList;
////        int i = daoStrategyService.saveDaoStrategyListOrUpdateDao(daoStrategyList, dao);
////        log.info("[CreateProjectParamEmittedForFundingChainService] daoId:{} return i:{}", dao.getId(), i);
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] daoId:{} Exception e:{}", dao.getDaoNumber(), e);
//        }
//        return null;
//    }
//
//
//    private String getFeeRecipient(String projectId) {
//        try {
//            InfuraCallRequestDto infuraCallRequestDto1 = new InfuraCallRequestDto();
//            infuraCallRequestDto1.setNetWork(ProtoDaoConstant.netWork);
//            infuraCallRequestDto1.setTo(ContractMethodEnum.FEE_RECEIPT.getContractAddress());
//            infuraCallRequestDto1.setData(ContractMethodEnum.FEE_RECEIPT.getMethodAddress() + projectId);
//
//            // 调用查询使用数据集的user
//            Result<String> resul1 = iSubscriptionService.infuraCall(infuraCallRequestDto1);
//            log.info("[CreateProjectParamEmittedForFundingChainService]infura FEE_RECEIPT return data:{}", resul1.getData());
//            if (resul1.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[CreateProjectParamEmittedForFundingChainService] FEE_RECEIPT error result:{}", resul1.getResultDesc());
//                throw new RuntimeException("保存project查询信息失败");
//            }
//
//            String projectInfoData1 = resul1.getData();
//            List<String> dataList1 = CommonUtil.splitBy32Bytes(projectInfoData1);
//            String fee_recipient = CommonUtil.formatBytes32Address(dataList1.get(0));
//            return fee_recipient;
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService]infura FEE_RECEIPT projectId:{} exception:{}", projectId, e);
//        }
//        return null;
//    }
//
//    /**
//     * 查询dao daoSymbol信息
//     *
//     * @param erc20_token
//     * @return
//     */
//    private String getDaoSymbol(String erc20_token) {
//        try {
//            InfuraCallRequestDto infuraCallRequestDto2 = new InfuraCallRequestDto();
//            infuraCallRequestDto2.setNetWork(ProtoDaoConstant.netWork);
//            infuraCallRequestDto2.setTo(erc20_token);
//            infuraCallRequestDto2.setData(ContractMethodEnum.DAO_SYMBOL.getMethodAddress());
//
//            // 调用查询使用数据集的user
//            Result<String> resul2 = iSubscriptionService.infuraCall(infuraCallRequestDto2);
//            if (resul2.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[CreateProjectParamEmittedForFundingChainService] dao symbol error result:{}", resul2.getResultDesc());
//                throw new RuntimeException("保存project查询symbol信息失败");
//            }
//            log.info("[CreateProjectParamEmittedForFundingChainService]infura symbol return data:{}", resul2.getData());
//            String projectInfoData2 = CommonUtil.removeHexPrefixIfExists(resul2.getData());
//            List<String> dataList2 = CommonUtil.splitBy32Bytes(projectInfoData2);
//            String daoSymbol = CommonUtil.dynamicArgumentDecoding(projectInfoData2, dataList2.get(0), true);
//            return daoSymbol;
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService]infura dao symbol erc20_token:{} exception:{}", erc20_token, e);
//        }
//        return null;
//    }
//
//    /**
//     * 查询erc20Name
//     *
//     * @param erc20_token
//     * @return
//     */
//    private String getErc20Name(String erc20_token) {
//        try {
//            InfuraCallRequestDto infuraCallRequestDto3 = new InfuraCallRequestDto();
//            infuraCallRequestDto3.setNetWork(ProtoDaoConstant.netWork);
//            infuraCallRequestDto3.setTo(erc20_token);
//            infuraCallRequestDto3.setData(ContractMethodEnum.NAME.getMethodAddress());
//
//            // 查询erc20Name
//            Result<String> resul3 = iSubscriptionService.infuraCall(infuraCallRequestDto3);
//            if (resul3.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[CreateProjectParamEmittedForFundingChainService] erc20 name error result:{}", resul3.getResultDesc());
//                throw new RuntimeException("保存project查询erc20name信息失败");
//            }
//            log.info("[CreateProjectParamEmittedForFundingChainService]infura erc20 name return data:{}", resul3.getData());
//            String projectInfoData3 = CommonUtil.removeHexPrefixIfExists(resul3.getData());
//            List<String> dataList3 = CommonUtil.splitBy32Bytes(projectInfoData3);
//            String erc20Name = CommonUtil.dynamicArgumentDecoding(projectInfoData3, dataList3.get(0), true);
//            return erc20Name;
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService]infura erc20 name erc20_token:{} exception:{}", erc20_token, e);
//        }
//        return null;
//    }
//
//
//    /**
//     * 查询 PROJECT_PRICE
//     *
//     * @param projectId
//     * @return
//     */
//    private String getProjectPrice(String projectId) {
//        InfuraCallRequestDto infuraCallPrice = new InfuraCallRequestDto();
//        infuraCallPrice.setNetWork(ProtoDaoConstant.netWork);
//        infuraCallPrice.setTo(ContractMethodEnum.PROJECT_PRICE.getContractAddress());
//        infuraCallPrice.setData(ContractMethodEnum.PROJECT_PRICE.getMethodAddress() + projectId);
//
//        // 调用查询price
//        Result<String> resultPrice = iSubscriptionService.infuraCall(infuraCallPrice);
//        if (resultPrice.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] price error result:{}", resultPrice.getResultDesc());
//            throw new RuntimeException("保存project查询price信息失败");
//        }
//        log.info("[CreateProjectParamEmittedForFundingChainService]price infura return data:{}", resultPrice.getData());
//        String priceData = resultPrice.getData();
//
//        priceData = CommonUtil.hexToTenString(priceData);
//        return priceData;
//
//    }
//
//    private void freshProjectUri(String projectUri, NewProjectUriDto newProjectUriDto) {
//        try {
//            String fileName = projectUri.substring(projectUri.lastIndexOf("/") + 1);
//            s3Service.deleteObject(
//                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName, fileName);
//            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
//            representaion.setObjectName(fileName);
//
//            newProjectUriDto.setDaoStart_date(null);
//            newProjectUriDto.setUser_address(null);
//            newProjectUriDto.setDiscord_link(null);
//            newProjectUriDto.setTwitter_link(null);
//            newProjectUriDto.setOpensea_link(null);
//            newProjectUriDto.setUri(null);
//            newProjectUriDto.setSocialLinks(null);
//            representaion.setText(JacksonUtil.obj2json(newProjectUriDto));
//
//            s3Service.putObject(ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName,
//                    representaion);
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] freshProjectUri projectUri:{} e:", projectUri, e);
//        }
//    }
//
//    private void subscribeForNewProject(Dao dao, TransactionDto transactionDto) {
//        // todo异步订阅
//        List<Subscribe> subscribeList = subscribeService.selectAll();
//        Subscribe subscribe1 =
//                subscribeList.stream().filter(v -> TradeTypeEnum.D4A_MINTNFT.getType().equals(v.getTradeType())
//                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());
//        Subscribe subscribe = new Subscribe();
//        subscribe.setContractAddress(dao.getErc721Token());
//        subscribe.setTopics(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
//        subscribe.setFromBlock(transactionDto.getBlockNumber());
//        subscribe.setReceiveAddress(subscribe1.getReceiveAddress());
//        subscribe.setTradeType(TradeTypeEnum.TRANSFER.getType());
//        subscribe.setOrderInit(subscribeList.size() + 1);
//        // 每分钟监听一次
//        subscribe.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());
//
//        SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
//        subscribeRequestDto.setAddress(dao.getErc721Token());
//        subscribeRequestDto.setFromBlock(transactionDto.getBlockNumber());
//        subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
//        subscribeRequestDto
//                .setTopics(Collections.singletonList(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress()));
//        subscribeRequestDto.setNoticeType(SubscriberTypeEnum.EVENT.getType());
//        subscribeRequestDto.setNoticeUrl(subscribe1.getReceiveAddress());
//        subscribeRequestDto.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
//        subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
//        try {
//            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[CreateProjectParamEmittedForFundingChainService] subscripe erc21 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] subscripe erc21 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//        subscribeService.save(subscribe);
//
//        // todo异步订阅
//        Subscribe subscribe2 = new Subscribe();
//        subscribe2.setContractAddress(dao.getErc20Token());
//        subscribe2.setTopics(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress());
//        subscribe2.setFromBlock(transactionDto.getBlockNumber());
//        subscribe2.setReceiveAddress(subscribe1.getReceiveAddress());
//        subscribe2.setTradeType(TradeTypeEnum.TRANSFER_ERC20.getType());
//        subscribe2.setOrderInit(subscribeList.size() + 2);
//        // 每分钟监听一次
//        subscribe2.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());
//
//        SubscribeRequestDto subscribeRequestDto2 = new SubscribeRequestDto();
//        subscribeRequestDto2.setAddress(dao.getErc20Token());
//        subscribeRequestDto2.setFromBlock(transactionDto.getBlockNumber());
//        subscribeRequestDto2.setNetwork(ProtoDaoConstant.netWork);
//        subscribeRequestDto2
//                .setTopics(Collections.singletonList(ContractMethodEnum.PROJECT_TRANSFER.getMethodAddress()));
//        subscribeRequestDto2.setNoticeType(SubscriberTypeEnum.BATCH_TRAN.getType());
//        subscribeRequestDto2.setNoticeUrl(subscribe1.getReceiveAddress());
//        subscribeRequestDto2.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
//        subscribeRequestDto2.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
//        try {
//            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto2);
//            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[CreateProjectParamEmittedForFundingChainService] subscripe erc20 retry error transactionDto:{} resultDesc:{}",
//                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
//                subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
//            } else {
//                subscribe2.setStatus(SubscribeStatusEnum.OPEN.getType());
//                subscribe2.setFilterId(subResult.getData());
//            }
//        } catch (Exception e) {
//            log.error("[CreateProjectParamEmittedForFundingChainService] subscripe erc20 retry error transactionDto:{} e:{}",
//                    JacksonUtil.obj2json(transactionDto), e);
//            subscribe2.setStatus(SubscribeStatusEnum.CLOSE.getType());
//        }
//        subscribeService.save(subscribe2);
//    }
//
//
//    public static void main(String[] args) {
//
//        String dataStr = "0xb03f0db702f34f65eafccb2849e0f9df06ce3343c021de28dcc86782aaaa38850000000000000000000000003264aa7c1e47657e5862652764cbcd22cc1185f700000000000000000000000042024629389dd303d2e332a2835e0908a545ba27000000000000000000000000c0b7c5ee97a35882eeab522515565992790539a8000000000000000000000000000000000000000000000000000000000000038000000000000000000000000000000000000000000000000000000000000004e00000000000000000000000000000000000000000000000000000000000000620000000000000000000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000000000000000000000007e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004e2000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000008c0000000000000000000000000000000000000000000000000000000000000001400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002616000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002616000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000012c000000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000009c400000000000000000000000000000000000000000000000000000000000012c00000000000000000000000000000000000000000000000000000000000002503000000000000000000000000000000000000000000000000000000000000001e000000000000000000000000000000000000000000000000000000000000270f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002ee00000000000000000000000000000000000000000000000000000000000000e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005e68747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f64616f2f66625735313941484554495443615742674c76367735383738322e6a736f6e00003f672c4ab3c5fbe23de6cca79930a5ca238b06d6cffc2367e48f5a1919638aa200000000000000000000000000000000000000000000000000000000000000801625d8fdf989774ecd7405db4874f9c8bb88e93aa6053b5d498f1cc12344f0d800000000000000000000000000000000000000000000000000000000000000e00000000000000000000000000000000000000000000000000000000000000002000000000000000000000000aa2ad3486c0c6d9089e6dde6ec10d2f56c8c88930000000000000000000000006d61ef08e6f0ede14f56a7b815e708a10c6356800000000000000000000000000000000000000000000000000000000000000002000000000000000000000000d2da81d325b61d05d00078a589ba7d42a34e154c000000000000000000000000f3d2154b09f35713786c00c031ab760caec7a1c4000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000200000000000000000000000091b8650f6e27d7855a23772cab9daa46a3fd4f1f000000000000000000000000e6046371b729f23206a94ddcace89fecebbd565c000000000000000000000000000000000000000000000000000000000000000100000000000000000000000091b8650f6e27d7855a23772cab9daa46a3fd4f1f000000000000000000000000000000000000000000000000000000000000007b00000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000002000000000000000000000000f8baf7268f3daefe4135f7711473ae8b6c3b47d8000000000000000000000000000000000000000000000000000000000000000b000000000000000000000000b90b90225e628188a16c1ab2ffbd8372e49b39df00000000000000000000000000000000000000000000000000000000000000160000000000000000000000000000000000000000000000000000000000000003000000000000000000000000d2da81d325b61d05d00078a589ba7d42a34e154c000000000000000000000000000000000000000000000000000000000000000b000000000000000000000000f3d2154b09f35713786c00c031ab760caec7a1c40000000000000000000000000000000000000000000000000000000000000016000000000000000000000000d105fb4719446921e2a619233641532600933d790000000000000000000000000000000000000000000000000000000000000021000000000000000000000000000000000000000000000000000000000000000087282c328f1533e7edc149b8ecd6162e14ee725df7e1104a0c5cb8f3fd86942b00000000000000000000000000000000000000000000000000000000000000800000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000008768747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f63616e7661732f383732383263333238663135333365376564633134396238656364363136326531346565373235646637653131303461306335636238663366643836393432622e6a736f6e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000157a746869726433333320696e3233206279333964660000000000000000000000";
//
//        String data = CommonUtil.removeHexPrefixIfExists(dataStr);
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//        String projectId = dataList.get(0);
//        String feePool = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
//        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
//        String erc721_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));
//
//        //1. 解析参数 daoMetadataParam
//        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(4)))) >> 5;
//        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(5)))) >> 5;
//        int c = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(6)))) >> 5;
//        int d = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(7)))) >> 5;
//        int e = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(8)))) >> 5;
//        //dao信息
//        String daoMetadataParam = "";
//        //dao 白名单信息
//        String whitelist = "";
//        //dao黑名单信息
//        String blacklist = "";
//        // dao铸造上限信息
//        String daoMintCapParam = "";
//        // nftMinterCapInfo
//        String nftMinterCapInfo = "";
//        for (int i = a; i < b; i++) {
//            daoMetadataParam += dataList.get(i);
//        }
//        for (int i = b; i < c; i++) {
//            whitelist += dataList.get(i);
//        }
//        for (int i = c; i < d; i++) {
//            blacklist += dataList.get(i);
//        }
//        for (int i = d; i < e; i++) {
//            daoMintCapParam += dataList.get(i);
//        }
//        for (int i = e; i < dataList.size(); i++) {
//            nftMinterCapInfo += dataList.get(i);
//        }
//
//        List<String> daoMetadataParamList = CommonUtil.splitBy32Bytes(daoMetadataParam);
//        String startDrb = CommonUtil.hexToTenString(daoMetadataParamList.get(0));
//        String mintableRounds = CommonUtil.hexToTenString(daoMetadataParamList.get(1));
//        //String floorPriceRank = CommonUtil.hexToTenString(daoMetadataParamList.get(2));
//        //String maxNftRank = CommonUtil.hexToTenString(daoMetadataParamList.get(3));
//        String royaltyFee = CommonUtil.hexToTenString(daoMetadataParamList.get(4));
//        String projectUri = CommonUtil.dynamicArgumentDecoding(daoMetadataParam, daoMetadataParamList.get(5), true);
//        //String projectIndex = CommonUtil.hexToTenString(daoMetadataParamList.get(6));
//
//
//        TransactionDto transactionDto = new TransactionDto();
//        transactionDto.setData(dataStr);
//        Dao dao = new Dao();
//
//        CreateProjectParamEmittedForFundingChainService createProjectParamEmittedForFundingChainService = new CreateProjectParamEmittedForFundingChainService();
//        //erc20 发放方式、价格变动及相关参数设置  templateParam
//        createProjectParamEmittedForFundingChainService.buildTemplateParam(transactionDto, dao);
//
//
//        List<DaoStrategy> daoStrategyList = new ArrayList<>();
//        //更新白名单信息 这里不处理，在WhitelistModifiedChainService服务里处理，创建dao的时候同时会触发这个事件
////        List<DaoStrategy> daoWhiteListStrategyList = buildWhitelist(transactionDto, whitelist, dao);
////        if (daoWhiteListStrategyList != null && daoWhiteListStrategyList.size() > 0) {
////            daoStrategyList.addAll(daoWhiteListStrategyList);
////        }
//
//        //更新黑名单信息 blacklist
//        List<DaoStrategy> daoBlackListStrategyList = createProjectParamEmittedForFundingChainService.buildBlacklist(transactionDto, blacklist, dao);
//        if (daoBlackListStrategyList != null && daoBlackListStrategyList.size() > 0) {
//            daoStrategyList.addAll(daoBlackListStrategyList);
//        }
//        //更新铸造上限 daoMintCapParam
////        List<DaoStrategy> daoMintCapStrategyList = createProjectParamEmittedForFundingChainService.buildDaoMintCapParam(transactionDto, daoMintCapParam, dao);
//
////        if (daoMintCapStrategyList != null && daoMintCapStrategyList.size() > 0) {
////            daoStrategyList.addAll(daoMintCapStrategyList);
////        }
//
//        // nftMinterCapInfo 待测试
//        List<DaoStrategy> nftMinterCapInfoStrategyList = createProjectParamEmittedForFundingChainService.buildNftMinterCapInfo(transactionDto, nftMinterCapInfo, dao);
//
////        if (nftMinterCapInfoStrategyList != null && nftMinterCapInfoStrategyList.size() > 0) {
////            daoStrategyList.addAll(nftMinterCapInfoStrategyList);
////        }
//        // basicDaoParam 信息先不处理
//        //actionType 它是合约调方法的一个判断符号，现在所有的创建应该都是默认20
//        // String actionType = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
//
//        //更新dao收益分配 allRatioForFundingParam
//        createProjectParamEmittedForFundingChainService.buildAllRatioForFundingParam(transactionDto, dao);
//
//
//    }
//}
