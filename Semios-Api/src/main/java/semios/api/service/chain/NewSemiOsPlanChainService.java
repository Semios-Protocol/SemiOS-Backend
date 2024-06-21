package semios.api.service.chain;


import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.request.SubscribeRequestDto;
import semios.api.model.dto.response.NewPlanUriDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.entity.Subscribe;
import semios.api.model.entity.TreasuryTransaction;
import semios.api.model.enums.*;
import semios.api.model.enums.Plan.PlanAmountSourceEnum;
import semios.api.model.enums.Plan.PlanRewardEnum;
import semios.api.model.enums.Plan.PlanStatusEnum;
import semios.api.model.enums.Plan.PlanTypeEnum;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * 新添加plan激励计划
 *
 * @description: 添加plan激励计划
 * @author: zhyyao
 * @create: 2024-05-01 13:43
 **/
@Slf4j
@Service
public class NewSemiOsPlanChainService implements SubscriberChainService {

    private static final RestTemplate REST_TEMPLATE = new RestTemplate();
    @Autowired
    private IIncentivePlanService iIncentivePlanService;
    @Autowired
    private S3Service s3Service;
    @Autowired
    private ISubscribeService subscribeService;
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private ITreasuryTransactionService treasuryTransactionService;
    @Autowired
    private CommonService commonService;
    @Value("${dao_default_logo}")
    private String daoDefaultLogo;

    /*
    bytes32 planId, //新建的这个plan的planId
    bytes32 daoId, //plan所属的 seed node的daoId
    uint256 startBlock, //plan开始的区块高度
    uint256 duration, //每个周期持续的区块数量
    uint256 totalRounds, //plan持续周期数
    uint256 totalReward, //发放总量，这个值在用户输入后需要乘以output token的decimal
    address rewardToken,// seed nodes erc20 token
    PlanTemplateType planTemplateType, //常数0
    bool io, //flase表示激励input token, true表示激励output token
    address owner,	//plan的创建者
    bool useTreasury, //是否使用国库
	string planUri //plan的相关信息
    **/

    public static void main(String[] args) {
        try {
            TransactionDto transactionDto = new TransactionDto();
            transactionDto.setData("0x883bcad60f1fb69ad5bc1be00919ad9e905015da22f996c133d49ca797b32db144a94e8f77846970c46715d527afcac36033d78f44d744a40dbf745072a1c03e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000180000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000052c88cf800000000000000000000000009fcfceb7c2f7f6748c082a95bdd925f1702a8c3a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000c537a223b7fe86483d31442248c5918177526bef00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000180000000000000000000000000000000000000000000000000000000000000006068747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f706c616e2f664534474e426e45567167685a57754f734f6464796730323030392e6a736f6e");

            NewSemiOsPlanChainService newSemiOsPlanChainService = new NewSemiOsPlanChainService();

            newSemiOsPlanChainService.handleTrade(transactionDto);
        } catch (Exception e) {
            log.info("error:{}", e.getMessage());
        }

    }

    @Override
    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[NewSemiOsPlanChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String planCode = dataList.get(0);
        String projectId = dataList.get(1);
        String startBlock = CommonUtil.hexToTenString(dataList.get(2));

        String totalRounds = CommonUtil.hexToTenString(dataList.get(4));    // plan的总周期数量
        String totalReward = CommonUtil.hexToTenString(dataList.get(5));    // 发放的总量,需要除以out put token的decimal

        String rewardToken = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(6)));   // reward token address

        int incentiveType = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(8))));  // 激励方式
        String createAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(9)));
        int isUseTreasury = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(10))));   // 是否使用国库

        Dao dao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus());
        // 如果刚创建dao，就创建plan，有些值可能没有，需要等待处理
        if (dao == null || StringUtils.isBlank(dao.getErc20Token()) || StringUtils.isBlank(dao.getPayCurrencyType()) || StringUtils.isBlank(dao.getDaoSymbol())) {
            log.error("[NewSemiOsPlanChainService] seed nodes is  not exits:" + projectId);
            throw new RuntimeException("seed nodes is  not exits:" + projectId);
        }

        String planUri = CommonUtil.dynamicArgumentDecoding(data, dataList.get(11), true);
        log.info("[NewSemiOsPlanChainService]uri:{}", planUri);

        IncentivePlan incentivePlan = iIncentivePlanService.planDetailByUri(planUri);
        if (incentivePlan != null) {
            log.error("[NewSemiOsPlanChainService]plan is exits uri:{}", planUri);
            throw new RuntimeException("plan is exits uri:" + planUri);
        }

        NewPlanUriDto newPlanUriDto = REST_TEMPLATE.getForObject(planUri, NewPlanUriDto.class);
        log.info("[NewSemiOsPlanChainService]uri return projectInfo:{}", JacksonUtil.obj2json(newPlanUriDto));
        if (newPlanUriDto == null) {
            log.error("[NewSemiOsPlanChainService] error newPlanUriDto is null transactionDto:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("newPlanUriDto is null");
        }

        if ("0".equals(startBlock)) {
            Result<String> resultBlock = iSubscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
            if (resultBlock.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[NewSemiOsPlanChainService] ethGetBlockNumber error:{}", JacksonUtil.obj2json(resultBlock));
            }
            startBlock = CommonUtil.hexToTenString(resultBlock.getData()); // 当前区块数
        }

        incentivePlan = new IncentivePlan();
        incentivePlan.setDaoId(dao.getId());
        incentivePlan.setProjectId(projectId);

        incentivePlan.setPlanCode(planCode);
        Integer planNumber = iIncentivePlanService.getPlanNumberByProjectId(projectId); // for update..是否启用
        log.info("[NewSemiOsPlanChainService] planNumber:{}", planNumber);
        incentivePlan.setPlanNumber(planNumber + 1);

        incentivePlan.setTransactionHash(transactionDto.getTransactionHash());

        Integer planStatus = getPlanStatus(planCode);
        incentivePlan.setIncentiveStatus(planStatus);
        incentivePlan.setCurrentRound(Objects.equals(planStatus, PlanStatusEnum.NOT_STARTED.getBasicType()) ? 0 : 1);

        incentivePlan.setPlanName(newPlanUriDto.getPlanName());
        incentivePlan.setPlanLogoUrl(daoDefaultLogo);

        incentivePlan.setIncentiveType(incentiveType == 0 ? PlanTypeEnum.INPUT_TOKEN.getBasicType() : PlanTypeEnum.OUTPUT_TOKEN.getBasicType());
        incentivePlan.setInputTokenSymbol(dao.getPayCurrencyType());
        incentivePlan.setOutputTokenSymbol(dao.getDaoSymbol());
        log.info("[NewSemiOsPlanChainService] incentivePlan{}", JacksonUtil.obj2json(incentivePlan));

        LocalDate dateParam;
        try {
            DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            dateParam = LocalDate.parse(newPlanUriDto.getPlanStartDate(), df);
        } catch (Exception ex) {
            log.error("[CreateProjectParamEmittedFourChainService] error dateParam is null daoStartDate:{}",
                    newPlanUriDto.getPlanStartDate());
            dateParam = null;
        }

        incentivePlan.setStartDate(dateParam);
        incentivePlan.setStartBlock(startBlock);
        incentivePlan.setAmountSource(isUseTreasury == 0 ? PlanAmountSourceEnum.WALLET.getBasicType() : PlanAmountSourceEnum.TREASURY.getBasicType());

        String decimal;

        // 需要除以out put token的decimal
        if (ProtoDaoConstant.ZERO_ADDRESS.equals(rewardToken) || rewardToken.equals(dao.getInputTokenAddress())) {
            incentivePlan.setRewardType(PlanRewardEnum.INPUT_TOKEN.getBasicType());
            incentivePlan.setRewardToken(dao.getInputTokenAddress());   // 如果是0地址或者其他，直接用dao.getInputTokenAddress()即可
            incentivePlan.setRewardTokenSymbol(dao.getPayCurrencyType());   // 默认为ETH，直接使用dao.getPayCurrencyType()即可
            incentivePlan.setRewardTokenDecimal(dao.getInputTokenDecimals());   // 默认为18，直接使用dao.getInputTokenDecimals()即可
        } else if (rewardToken.equals(dao.getErc20Token())) {
            decimal = commonService.erc20Decimals(rewardToken);

            incentivePlan.setRewardType(PlanRewardEnum.OUTPUT_TOKEN.getBasicType());
            incentivePlan.setRewardToken(rewardToken);
            incentivePlan.setRewardTokenSymbol(dao.getDaoSymbol()); // 这个字段为erc20的symbol
            incentivePlan.setRewardTokenDecimal(Integer.valueOf(decimal));
        } else {
            // 第三方..
            decimal = commonService.erc20Decimals(rewardToken);
            String rewardTokenSymbol = getDaoSymbol(rewardToken);

            incentivePlan.setRewardType(PlanRewardEnum.CUSTOM_TOKEN.getBasicType());
            incentivePlan.setRewardToken(rewardToken);
            incentivePlan.setRewardTokenSymbol(rewardTokenSymbol);
            incentivePlan.setRewardTokenDecimal(Integer.valueOf(decimal));
        }


        // TODO 需要调整....
        BigDecimal incentiveAmount = null;
        if (totalReward != null) {
            incentiveAmount = new BigDecimal(totalReward).divide(CommonUtil.getPowBigDecimal(incentivePlan.getRewardTokenDecimal()), 18, RoundingMode.HALF_UP);
        }
        incentivePlan.setIncentiveAmount(incentiveAmount);
        incentivePlan.setRemainingToken(incentiveAmount);   // 初始化数据，默认为总金额
        if (totalRounds != null) {
            incentivePlan.setPlanBlockWindow(Integer.valueOf(totalRounds));
        }
        incentivePlan.setDuration(newPlanUriDto.getDuration());


        incentivePlan.setCreateBy(createAddress);
        incentivePlan.setPlanUri(planUri);
        // incentivePlan.setCreateTime(LocalDateTime.now());

        // 如果使用了国库打钱，需要添加一条记录
        log.info("[CreateProjectParamEmittedFourChainService] check is use treasury:{}", JacksonUtil.obj2json(incentivePlan));
        if (incentivePlan.getAmountSource().equals(PlanAmountSourceEnum.TREASURY.getBasicType())) {
            addRecordTreasury(dao, transactionDto, incentiveAmount);
        }

        // 添加获取plan周期的订阅
        log.info("[CreateProjectParamEmittedFourChainService] add new sub:{}", JacksonUtil.obj2json(incentivePlan));
        subscribeForNewProject(incentivePlan, transactionDto);


        // 清空plan uri
        // freshPlanUri(planUri,newPlanUriDto);

        log.info("[CreateProjectParamEmittedFourChainService] insert plan info:{}", JacksonUtil.obj2json(incentivePlan));
        iIncentivePlanService.save(incentivePlan);
    }

    private void addRecordTreasury(Dao dao, TransactionDto transactionDto, BigDecimal incentiveAmount) {
        TreasuryTransaction treasuryTransaction = new TreasuryTransaction();

        treasuryTransaction.setProjectId(dao.getProjectId());   // main dao的projectId
        treasuryTransaction.setTransactionHash(transactionDto.getTransactionHash());
        treasuryTransaction.setToAddress(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getContractAddress());    // 主合约地址
        treasuryTransaction.setBlockNumber(CommonUtil.hexToTenString(transactionDto.getBlockNumber()));
        treasuryTransaction.setGenerateErc721Address(null);
        treasuryTransaction.setGenerateTokenId(null);
        treasuryTransaction.setAmount(incentiveAmount);
        treasuryTransaction.setIsUseTreasury(1);    // 使用国库

        // 使用国库地址
        treasuryTransaction.setFromAddress(CommonUtil.addHexPrefixIfNotExist(dao.getTreasuryErc20()));
        treasuryTransaction.setTransactionType(TreasuryTransactionTypeEnum.TO_SUB_DAO.getStatus()); // 国库出钱
        treasuryTransaction.setSubDaoProjectId(null);

        log.info("[addRecordTreasury] insert treasury info:{}", JacksonUtil.obj2json(treasuryTransaction));
        treasuryTransactionService.save(treasuryTransaction);
    }

    // 实时获取plan的状态
    private Integer getPlanStatus(String planId) {
        log.info("[getPlanStatus] planCode:{}", planId);
        Integer currentRound = commonService.getPlanCurrentRound(planId);
        if (currentRound == 0) {
            return PlanStatusEnum.NOT_STARTED.getBasicType();
        }
        return PlanStatusEnum.STARTED.getBasicType();

    }

    // 不用清空....
    private void freshPlanUri(String planUri, NewPlanUriDto newPlanUriDto) {
        try {
            String fileName = planUri.substring(planUri.lastIndexOf("/") + 1);
            s3Service.deleteObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.planBucketName, fileName);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);

            newPlanUriDto.setPlanLogo(null);
            newPlanUriDto.setPlanName(null);
            newPlanUriDto.setDuration(null);
            newPlanUriDto.setPlanStartDate(null);
            newPlanUriDto.setUserAddress(null);

            representaion.setText(JacksonUtil.obj2json(newPlanUriDto));

            s3Service.putObject(ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.daoBucketName,
                    representaion);
        } catch (Exception e) {
            log.error("[NewSemiOsPlanChainService] freshProjectUri projectUri:{} e:", planUri, e);
        }
    }

    private void subscribeForNewProject(IncentivePlan incentivePlan, TransactionDto transactionDto) {
        // todo异步订阅
        List<Subscribe> subscribeList = subscribeService.selectAll();

        // 根据current来获取receive address
        Subscribe subscribex =
                subscribeList.stream().filter(v -> TradeTypeEnum.CURRENT_ROUND.getType().equals(v.getTradeType())
                        && StringUtils.isNotBlank(v.getReceiveAddress())).findFirst().orElse(new Subscribe());

        // todo异步订阅  getPlanCurrentRound
        Subscribe subscribe = new Subscribe();
        subscribe.setContractAddress(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getContractAddress());   // 合约要调整
        subscribe.setTopics(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getMethodAddress() + incentivePlan.getPlanCode());
        subscribe.setFromBlock(transactionDto.getBlockNumber());
        subscribe.setReceiveAddress(subscribex.getReceiveAddress());
        subscribe.setTradeType(TradeTypeEnum.getPlanCurrentRound.getType());
        subscribe.setOrderInit(subscribeList.size() + 1);
        // 每分钟监听一次
        subscribe.setIntervalTime(SubIntervalTimeEnum.SIXTY.getTime());

        // 立即订阅一次
        SubscribeRequestDto subscribeRequestDto = new SubscribeRequestDto();
        subscribeRequestDto.setAddress(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getContractAddress());
        // subscribeRequestDto.setFromBlock(transactionDto.getBlockNumber());
        subscribeRequestDto.setNetwork(ProtoDaoConstant.netWork);
        subscribeRequestDto
                .setTopics(Collections.singletonList(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getMethodAddress() + incentivePlan.getPlanCode()));

        subscribeRequestDto.setNoticeType(SubscriberTypeEnum.VALUE.getType());
        subscribeRequestDto.setNoticeUrl(subscribex.getReceiveAddress());
        subscribeRequestDto.setIntervalPeriod(SubIntervalTimeEnum.SIXTY.getTime());
        subscribeRequestDto.setAppName(ProtoDaoConstant.netWork + "-" + "protodao");
        log.info("[NewSemiOsPlanChainService] create sub now:" + JacksonUtil.obj2json(subscribeRequestDto));
        try {
            Result<String> subResult = iSubscriptionService.subscripe(subscribeRequestDto);
            if (subResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[NewSemiOsPlanChainService] subscripe GET_PLAN_CURRENT_ROUND retry error transactionDto:{} resultDesc:{}",
                        JacksonUtil.obj2json(transactionDto), subResult.getResultDesc());
                subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
            } else {
                subscribe.setStatus(SubscribeStatusEnum.OPEN.getType());
                subscribe.setFilterId(subResult.getData());
            }
        } catch (Exception e) {
            log.error("[NewSemiOsPlanChainService] subscripe GET_PLAN_CURRENT_ROUND retry error transactionDto:{} e:{}",
                    JacksonUtil.obj2json(transactionDto), e.getMessage());
            subscribe.setStatus(SubscribeStatusEnum.CLOSE.getType());
        }
        log.info("subscribeForNewProject save subscribe:" + JacksonUtil.obj2json(subscribe));
        subscribeService.save(subscribe);
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
}
