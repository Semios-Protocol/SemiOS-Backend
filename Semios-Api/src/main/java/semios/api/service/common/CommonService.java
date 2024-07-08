package semios.api.service.common;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.model.bo.JsonRpcBo;
import semios.api.model.bo.JsonRpcParamBo;
import semios.api.model.bo.WorkCountBo;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.response.MintWorkUriDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.model.vo.req.WorkCreateReqVo;
import semios.api.model.vo.res.BasicInformationVo;
import semios.api.model.vo.res.MintWindowInfoVo;
import semios.api.service.*;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.ImageUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;

import java.io.File;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description: common service
 * @author: xiangbin
 * @create: 2022-09-20 18:39
 **/
@Slf4j
@Service
public class CommonService {

    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;

    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ICanvasService canvasService;

    @Autowired
    private IWorkService workService;

    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    @Autowired
    private ISubscribeService subscribeService;

    @Autowired
    private IUserHarvestTokenService userHarvestTokenService;

    @Value("${user_profile_image}")
    private String headImage;

    @Autowired
    private S3Service s3Service;

    @Value("${work_image_url}")
    private String workImageUrl;

    @Value("${dao_default_logo}")
    private String daoDefaultLogo;

    @Value("${multichain_url}")
    private String multichainUrl;

    @Value("${multichain_chain}")
    private String multichainChain;

    @Value("${multichain_key}")
    private String multichainKey;

    @Value("${bitquery_url}")
    private String bitqueryUrl;

    @Value("${bitquery_api_key}")
    private String bitqueryApiKey;

    @Value("${query_token_receiver}")
    private String queryTokenReceiver;

    @Value("${query_token_sender}")
    private String queryTokenSender;

    @Value("${query_eth_receiver}")
    private String queryEthReceiver;

    @Value("${query_eth_sender}")
    private String queryEthSender;

    @Value("${query_eth_token}")
    private String queryEthToken;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    public static void main(String[] args) throws Exception {
//        String requestParam = String.format("{\"query\":\"query MyQuery($network:EthereumNetwork,$tokenAddress:String,$receiver:String,$startTime:ISO8601DateTime,$endTime:ISO8601DateTime) {\\n  ethereum(network: $network) {\\n    transfers(\\n      sender: {is: $receiver}\\n      currency: {is: $tokenAddress}\\n      time: {since: $startTime, till: $endTime}\\n      amount: {gt: 0}\\n    ) {\\n      amount(calculate: sum)\\n    }\\n  }\\n}\\n\",\"variables\":{\"network\":\"%s\",\"startTime\":\"%sT00:00:00Z\",\"endTime\":\"2%sT00:00:00Z\",\"receiver\":\"%s\",\"tokenAddress\":\"%s\"}}", ProtoDaoConstant.netWork, "2024-01-15", "2024-01-16", "0x109f6010c83c720e8c67a902e5269680e2d6da6e", "0xf3452b81d6a2f962dbf688b58a86e9653adcc69e");
//        String requestParam = "{\\\"query\\\":\\\"query MyQuery($network:EthereumNetwork,$tokenAddress:String,$receiver:String,$startTime:ISO8601DateTime,$endTime:ISO8601DateTime) {\\n  ethereum(network: $network) {\\n    transfers(\\n      sender: {is: $receiver}\\n      currency: {is: $tokenAddress}\\n      time: {since: $startTime, till: $endTime}\\n      amount: {gt: 0}\\n    ) {\\n      amount(calculate: sum)\\n    }\\n  }\\n}\\n\\\",\\\"variables\\\":{\\\"network\\\":\\\"goerli\\\",\\\"startTime\\\":\\\"2024-01-15T00:00:00Z\\\",\\\"endTime\\\":\\\"22024-01-16T00:00:00Z\\\",\\\"receiver\\\":\\\"0x8c17f639c2c1f9f8a836ae781440dc6ed274537f\\\",\\\"tokenAddress\\\":\\\"0xf3452b81d6a2f962dbf688b58a86e9653adcc69e\\\"}}";
//        requestParam = requestParam.replaceAll("\\\\", "");
////                requestParam = URLEncoder.encode(requestParam, "utf8");
//        System.out.println(requestParam);

        String requestParam = "{\"query\":\"query ($network: EthereumNetwork!, $from: ISO8601DateTime, $to: ISO8601DateTime, $receiver: String,$token: [String!] ) {\\n  ethereum(network: $network) {\\n    transfers(\\n      receiver: {is: $receiver}\\n      currency: {in: $token}\\n      time: {since: $from, till: $to}\\n      options: {asc: qwert\"currency.symbolqwert\"}\\n    ) {\\n      amount(calculate: sum)\\n      currency {\\n        symbol\\n      }\\n    }\\n  }\\n}\\n\",\"variables\":\"{\\n  \\\"network\\\": \\\"%s\\\",\\n  \\\"from\\\": \\\"%sT00:00:00Z\\\",\\n  \\\"to\\\": \\\"%sT00:00:00Z\\\",\\n  \\\"receiver\\\":\\\"%s\\\",\\n  \\\"token\\\":\\\"%s\\\"\\n}\"}";

        requestParam = requestParam.replaceAll("qwert", "\\\\");
        requestParam = requestParam.replaceAll("\n", "");
        System.out.println(requestParam);
    }

    public void handleDaoDrbStatistics(List<DaoDrbStatistics> daoDrbStatisticsList, Integer drbNumber) {
        // ****注意*****不包括轮空区块  1.9改为包括轮空区块了
        for (DaoDrbStatistics daoDrbStatistics : daoDrbStatisticsList) {
            try {
                if (daoDrbStatistics.getTimes() != null && daoDrbStatistics.getTimes() >= 2) {
                    log.error("[handleDaoDrbStatistics]error times is over two times drbNumber:{} daoDrbStatistics:{}", drbNumber,
                            JacksonUtil.obj2json(daoDrbStatistics));
                }

                Dao dao = daoService.getById(daoDrbStatistics.getDaoId());
                if (dao == null) {
                    log.error("[handleDaoDrbStatistics]drbNumber:{} dao is null daoId:{} daoDrbStatisticsId:{} ", drbNumber, daoDrbStatistics.getDaoId(), daoDrbStatistics.getId());
                    continue;
                }
                Integer canvasAmount = canvasService.listCanvasAmountByDaoId(dao.getId() + "");

                daoDrbStatistics.setDaoId(dao.getId());
                daoDrbStatistics.setDrbNumber(drbNumber);

                List<DaoDrbStatistics> daoDrbStatisticsList1 =
                        daoDrbStatisticsService.selectGalleryDao(drbNumber - 6, drbNumber);
                BigDecimal sevenDayDrbVol =
                        daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(daoDrbStatistics.getDaoId()))
                                .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                daoDrbStatistics.setSevenDayDrbVol(sevenDayDrbVol);

                // 调用查询使用数据集的user
                try {
//                    Result<String> result = iSubscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
//                    if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
//                        log.info("[handleDaoDrbStatistics]infura ethGetBalance return data:{}", result.getData());
//                        String balance = result.getData();
//                        String price = CommonUtil.hexToTenString(balance);
//                        if (StringUtils.isNotBlank(price)) {
//                            daoDrbStatistics.setDaoAssetPool(new BigDecimal(price)
//                                    .divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR));
//                            dao.setDaoAssetPool(new BigDecimal(price)
//                                    .divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR));
//                        }
//                    } else {
//                        log.error("[handleDaoDrbStatistics]infura ethGetBalance return data:{} daoId:{}", result.getData(),
//                                daoDrbStatistics.getDaoId());
//                    }
                    dao.setDaoAssetPool(this.getInputToken(dao));

                    // dao的protocol合约的balance方法
                    InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                    infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                    infuraCallRequestDto.setTo(ContractMethodEnum.CLAIM_DAO_CREATOR_REWARD_FUNDING.getContractAddress());
                    infuraCallRequestDto.setData(ContractMethodEnum.CLAIM_DAO_CREATOR_REWARD_FUNDING.getMethodAddress() + dao.getProjectId());

                    // 调用查询使用数据集的user
                    Result<String> claimResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
                    if (claimResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                        log.info("[handleDaoDrbStatistics]infura daoId:{} PROJECT_CLAIM_ETH return data:{}", dao.getId(), claimResult.getData());
                        List<String> dataList = CommonUtil.splitBy32Bytes(claimResult.getData());
                        String daoCreatorERC20Reward = CommonUtil.hexToTenString(dataList.get(0));
                        String daoCreatorETHReward = CommonUtil.hexToTenString(dataList.get(1));
//                        String claim = claimResult.getData();
//                        String claimPrice = CommonUtil.hexToTenString(claim);
                        // 更细未领取代币数量
                        dao.setUnclaimedToken(
                                new BigDecimal(daoCreatorERC20Reward).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())));
                        dao.setUnclaimedEth(
                                new BigDecimal(daoCreatorETHReward).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
                    } else {
                        log.error("[handleDaoDrbStatistics]infura PROJECT_CLAIM return data:{} desc:{} daoId:{}",
                                claimResult.getData(), claimResult.getResultDesc(), daoDrbStatistics.getDaoId());
                    }

                } catch (Exception e) {
                    log.error("[handleDaoDrbStatistics]infura ethGetBalance error daoId:{} e:{} ", dao.getId(), e);
                }

                daoDrbStatistics.setCanvas(canvasAmount);
                Integer ownerAmount = workService.selectNftOwners(dao.getId() + "");
                Integer nftAmount = workService.selectNftAmounts(dao.getId() + "");
                Integer workAmount = workService.selectWorkAmounts(dao.getId() + "");
                Canvas canvas1 = canvasService.listCanvasFloorPriceByDaoId(dao.getId() + "");
                if (canvas1 != null) {
                    daoDrbStatistics.setFloorPrice(canvas1.getCurrentPrice());
                }
                if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                    daoDrbStatistics.setFloorPrice(dao.getGlobalDaoPrice());
                }
                daoDrbStatistics.setOwners(ownerAmount + "");
                daoDrbStatistics.setNft(nftAmount + "");
                daoDrbStatistics.setWorks(workAmount);

                daoDrbStatistics.setDaoName(dao.getDaoName());
                daoDrbStatistics.setDaoLogourl(dao.getDaoLogoUrl());
                daoDrbStatistics.setDaoDescription(dao.getDaoDescription());
                daoDrbStatistics.setDaoNumber(dao.getDaoNumber() + "");
                // 查询canvas下铸造总收入
                CanvasDrbStatistics canvasDrbStatistics =
                        canvasDrbStatisticsService.selectMintRevenueByDaoId(dao.getId(), drbNumber);
                BigDecimal mintRevenue = BigDecimal.ZERO;
                BigDecimal mintRevenueExTax = BigDecimal.ZERO;
                if (canvasDrbStatistics != null && canvasDrbStatistics.getSevenDayDrbVol() != null) {
                    mintRevenue = canvasDrbStatistics.getSevenDayDrbVol();
                }
                if (canvasDrbStatistics != null && canvasDrbStatistics.getMintRevenueExTax() != null) {
                    mintRevenueExTax = canvasDrbStatistics.getMintRevenueExTax();
                }
                log.info("[handleDaoDrbStatistics]mintRevenue:{}, daoId:{}", mintRevenue, dao.getId());
                daoDrbStatistics.setMintRevenue(mintRevenue);
                daoDrbStatistics.setMintRevenueExTax(mintRevenueExTax);
                // 计算未来收益 dao一共的区块数/dao已经存在的区块数 * mint revenue

                Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
                Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
                long blockNumber = daoDrbStatisticsPage.getTotal() + 1;
                BigDecimal dre = new BigDecimal(dao.getDaoMintWindow())
                        .divide(new BigDecimal(blockNumber), 4, RoundingMode.FLOOR).multiply(daoDrbStatistics.getMintRevenue());
                daoDrbStatistics.setDre(dre.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString());

                //1.4 当前window分配的token和eth计算
                List<DaoAllocationStrategy> daoAllocationStrategies = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
                DaoAllocationStrategy optional = new DaoAllocationStrategy();
                DaoAllocationStrategy daoAllocationStrategyToken = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.FALSE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.THREE.getType().equals(v.getRoyaltyType())).findFirst().orElse(optional);
                DaoAllocationStrategy daoAllocationStrategyEth = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.THREE.getType().equals(v.getRoyaltyType())).findFirst().orElse(optional);

                BigDecimal royaltyProportion = daoAllocationStrategyToken.getRoyaltyProportion() == null ? BigDecimal.ZERO : daoAllocationStrategyToken.getRoyaltyProportion();
                BigDecimal ethRoyaltyProportion = daoAllocationStrategyEth.getRoyaltyProportion() == null ? BigDecimal.ZERO : daoAllocationStrategyEth.getRoyaltyProportion();
                daoDrbStatistics.setAssetPoolTokenCost(getRoundErc20Reward(dao, drbNumber).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(new BigDecimal("100").subtract(royaltyProportion))));
                daoDrbStatistics.setAssetPoolEthCost(getRoundEthReward(dao, drbNumber).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(new BigDecimal("100").subtract(ethRoyaltyProportion))));

                // 已废弃：计算方法， dao经过的drb/总的drb * 总的发放量
                if (dao.getDaoVersion() == 3) {
                    try {
                        // 获取指定 DAO 和 round 的直到 round（包括）的 ERC20 奖励
                        // function getRewardTillRound(bytes32 daoId, uint256 round) public view returns (uint256);
                        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                        infuraCallRequestDto.setTo(ContractMethodEnum.GET_ERC_20_REWARD_TILL_ROUND.getContractAddress());
                        infuraCallRequestDto.setData(ContractMethodEnum.GET_ERC_20_REWARD_TILL_ROUND.getMethodAddress() + dao.getProjectId() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(Integer.parseInt(dao.getCurrentRound()))));

                        // 调用查询使用数据集的user
                        Result<String> getRewardTillRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
                        if (getRewardTillRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                            log.info("[handleDaoDrbStatistics]infura getRewardTillRoundResult return data:{}", getRewardTillRoundResult.getData());
                            String claim = getRewardTillRoundResult.getData();
                            String claimPrice = CommonUtil.hexToTenString(claim);
                            daoDrbStatistics.setDaoReward(new BigDecimal(claimPrice).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())));
                        } else {
                            log.error("[handleDaoDrbStatistics]infura getRewardTillRoundResult return data:{} desc:{} daoId:{}",
                                    getRewardTillRoundResult.getData(), getRewardTillRoundResult.getResultDesc(), daoDrbStatistics.getDaoId());
                        }

                    } catch (Exception e) {
                        log.error("[handleDaoDrbStatistics]infura getRewardTillRoundResult error daoId:{} e:{} ", dao.getId(), e);
                        //如果失败了先置为0，之后修改数据库
                        daoDrbStatistics.setDaoReward(BigDecimal.ZERO);
                    }
                } else {
                    // 计算方法， dao经过的drb/总的drb * 总的发放量
                    Integer mintWindow = dao.getDaoMintWindow();
                    String totalSupply = dao.getErc20TotalSupply();

                    daoDrbStatistics
                            .setDaoReward(new BigDecimal(blockNumber).divide(new BigDecimal(mintWindow), 18, RoundingMode.FLOOR)
                                    .multiply(new BigDecimal(totalSupply)).setScale(2, RoundingMode.FLOOR));
                }


                dao.setDaoReward(daoDrbStatistics.getDaoReward());
                List<Canvas> canvasList = canvasService.listCanvasByDaoId(dao.getId() + "");
                BigDecimal canvsSwopToken = canvasList.stream().filter(v -> v.getSwapToken() != null)
                        .map(Canvas::getSwapToken).reduce(BigDecimal.ZERO, BigDecimal::add);
                dao.setUnchangedTokenAmount(daoDrbStatistics.getDaoReward()
                        .subtract(new BigDecimal(String.valueOf(dao.getSwapToken() == null ? 0 : dao.getSwapToken())))
                        .subtract(new BigDecimal(String.valueOf(dao.getTransferToken() == null ? 0 : dao.getTransferToken())))
                        .subtract(canvsSwopToken));

                daoDrbStatistics.setStatus(StatisticsStatusEnum.JSWC.getStatus());
                log.info("[handleDaoDrbStatistics] drbNumber:{} dao:{} daoDrbStatistics:{}", drbNumber, JacksonUtil.obj2json(dao), JacksonUtil.obj2json(daoDrbStatistics));
                int i = daoDrbStatisticsService.updateDaoAndDaoDrbStatistics(daoDrbStatistics, dao);
                log.info("[handleDaoDrbStatistics] drbNumber:{} daoId:{} i:{}", drbNumber, dao.getId(), i);
            } catch (Exception e) {
                log.error("[handleDaoDrbStatistics] drbNumber:{} daoDrbStatisticsId:{} e:", drbNumber, daoDrbStatistics.getId(), e);
            }
        }
    }

    public void handleCanvasDrbStatistics(List<CanvasDrbStatistics> canvasDrbStatisticsList, Integer drbNumber) {

        for (CanvasDrbStatistics canvasDrbStatistics : canvasDrbStatisticsList) {

            try {
                if (canvasDrbStatistics.getTimes() != null && canvasDrbStatistics.getTimes() >= 2) {
                    log.error("[handleCanvasDrbStatistics]error times is over two times canvasDrbStatistics:{}",
                            JacksonUtil.obj2json(canvasDrbStatistics));
                }

                Canvas canvas = canvasService.getById(canvasDrbStatistics.getCanvasId());
                Dao dao = daoService.getById(canvas.getDaoId());

                canvasDrbStatistics.setCanvasId(canvas.getId());
                canvasDrbStatistics.setDaoId(canvas.getDaoId());
                canvasDrbStatistics.setProjectId(canvas.getProjectId());
                canvasDrbStatistics.setDrbNumber(drbNumber);

                Double drbVol =
                        workService.selectNftMintedPriceByDrbAndCanId(canvasDrbStatistics.getCanvasId() + "", drbNumber + "");
                if (drbVol == null) {
                    drbVol = 0.0;
                }
                canvasDrbStatistics.setDrbVol(new BigDecimal(String.valueOf(drbVol)));
                Integer startDrb = drbNumber >= 6 ? drbNumber - 6 : 0;
                BigDecimal canvasSevenDayDrbVol = BigDecimal.ZERO;
                CanvasDrbStatistics canvasDrbStatistics1 = canvasDrbStatisticsService
                        .selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", startDrb, drbNumber);
                if (canvasDrbStatistics1 != null && canvasDrbStatistics1.getSevenDayDrbVol() != null) {
                    canvasSevenDayDrbVol = canvasDrbStatistics1.getSevenDayDrbVol();
                    canvasDrbStatistics.setSevenDayDrbVol(canvasSevenDayDrbVol);
                }

                CanvasDrbStatistics canvasDrbStatistics2 =
                        canvasDrbStatisticsService.selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", 0, drbNumber);
                if (canvasDrbStatistics2 != null && canvasDrbStatistics2.getSevenDayDrbVol() != null) {
                    canvasDrbStatistics.setTotalVol(canvasDrbStatistics2.getSevenDayDrbVol());
                }
                List<DaoDrbStatistics> daoDrbStatisticsList1 =
                        daoDrbStatisticsService.selectGalleryDao(startDrb, drbNumber);
                BigDecimal sevenDayDrbVol =
                        daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(canvasDrbStatistics.getDaoId()))
                                .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);

                DaoDrbStatistics daoDrbStatistics =
                        daoDrbStatisticsService.selectByDaoIdAndDrbNumber(canvasDrbStatistics.getDaoId(), drbNumber);
                if (daoDrbStatistics != null && daoDrbStatistics.getDrbVol() != null && drbVol.compareTo(0.0) > 0
                        && daoDrbStatistics.getDrbVol().compareTo(BigDecimal.ZERO) > 0) {
                    canvasDrbStatistics.setNtvr(
                            new BigDecimal(String.valueOf(drbVol)).divide(daoDrbStatistics.getDrbVol(), 2, RoundingMode.FLOOR));
                } else {
                    //铸造价格为零的情况，查询canvas铸造数量/dao铸造数量
                    int canvasCount = workService.selectDrbNftCountByCanvasId(canvasDrbStatistics.getCanvasId() + "", drbNumber);
                    int daoCount = workService.selectDrbNftCountByDaoId(canvasDrbStatistics.getDaoId() + "", drbNumber);
                    if (canvasCount > 0 && daoCount > 0) {
                        canvasDrbStatistics.setNtvr(
                                new BigDecimal(String.valueOf(canvasCount)).divide(new BigDecimal(String.valueOf(daoCount)), 2, RoundingMode.FLOOR));
                    }
                }
                if (canvasSevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0 && sevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0) {
                    canvasDrbStatistics.setSevenDayNtrv(canvasSevenDayDrbVol.divide(sevenDayDrbVol, 4, RoundingMode.FLOOR));
                } else {
                    int canvasCount = workService.selectRangeNftCountByCanvasId(canvas.getId() + "", startDrb, drbNumber);
                    int daoCount = workService.selectRangeNftCountByDaoId(dao.getId() + "", startDrb, drbNumber);
                    if (canvasCount > 0 && daoCount > 0) {
                        canvasDrbStatistics.setSevenDayNtrv(new BigDecimal(String.valueOf(canvasCount)).divide(new BigDecimal(String.valueOf(daoCount)), 4, RoundingMode.FLOOR));
                    } else {
                        canvasDrbStatistics.setSevenDayNtrv(BigDecimal.ZERO);
                    }
                }
                if (canvasDrbStatistics2 != null && canvasDrbStatistics2.getSevenDayDrbVol() != null) {
                    canvasDrbStatistics.setMintRevenue(canvasDrbStatistics2.getSevenDayDrbVol());
                }
                if (canvasDrbStatistics2 != null && canvasDrbStatistics2.getMintRevenueExTax() != null) {
                    canvasDrbStatistics.setMintRevenueExTax(canvasDrbStatistics2.getMintRevenueExTax());
                }

                InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                infuraCallRequestDto.setTo(ContractMethodEnum.CLAIM_CANVAS_REWARD_FUNDING.getContractAddress());
                infuraCallRequestDto
                        .setData(ContractMethodEnum.CLAIM_CANVAS_REWARD_FUNDING.getMethodAddress() + canvas.getCanvasId());

                if (!canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
                    try {
                        // 调用查询使用数据集的user
                        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                            log.error("[handleCanvasDrbStatistics] error result:{} canvasId:{}", result.getResultDesc(),
                                    canvas.getId());
                            // throw new RuntimeException("保存canvas查询价格信息失败");
                            continue;
                        }
                        log.info("[handleCanvasDrbStatistics]infura claimCanvasRewardFunding canvasId:{} return data:{}", canvas.getId(), result.getData());
//                        String canvasInfoData = result.getData();
//                        String price = CommonUtil.hexToTenString(canvasInfoData);
                        List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
                        String canvasCreatorERC20Reward = CommonUtil.hexToTenString(dataList.get(0));
                        String canvasCreatorETHReward = CommonUtil.hexToTenString(dataList.get(1));
                        if (StringUtils.isNotBlank(canvasCreatorERC20Reward) && !"0".equals(canvasCreatorERC20Reward)) {
                            BigDecimal token = new BigDecimal(canvasCreatorERC20Reward).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()));
                            canvas.setUnclaimedToken(token);
                            BigDecimal receivedToken = canvas.getReceivedToken();
                            if (receivedToken == null) {
                                receivedToken = BigDecimal.ZERO;
                            }
                            BigDecimal swapToken = canvas.getSwapToken();
                            if (swapToken == null) {
                                swapToken = BigDecimal.ZERO;
                            }
                            BigDecimal transferToken = canvas.getTransferToken();
                            if (transferToken == null) {
                                transferToken = BigDecimal.ZERO;
                            }
                            canvasDrbStatistics.setDaoReward(token.add(receivedToken).add(swapToken).add(transferToken));
                        }
                        if (StringUtils.isNotBlank(canvasCreatorETHReward) && !"0".equals(canvasCreatorETHReward)) {
                            BigDecimal eth = new BigDecimal(canvasCreatorETHReward).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()));
                            canvas.setUnclaimedEth(eth);
                        }
                    } catch (Exception e) {
                        log.error("[currentDrbCanvasStatistics]infura error canvasId:{} e:{}",
                                canvasDrbStatistics.getCanvasId(), e);
                    }
                } else {
                    // 查询上一个drb的
                    CanvasDrbStatistics canvasDrbStatistics3 =
                            canvasDrbStatisticsService.selectLastedByCanvasId(canvas.getId());
                    if (canvasDrbStatistics3 != null) {
                        canvasDrbStatistics.setDaoReward(canvasDrbStatistics3.getDaoReward());
                    }
                }

                // canvas一定会存在的区块数/canvas已经存在的区块数 * mint revenue
                Page<CanvasDrbStatistics> iCanvasPage = new Page<>(1, 10);
                Page<CanvasDrbStatistics> canvasDrbStatisticsPage =
                        canvasDrbStatisticsService.selectByCanvasId(iCanvasPage, canvas.getId() + "");
                long blockNumber = canvasDrbStatisticsPage.getTotal() + 1;
                BigDecimal dre =
                        new BigDecimal(canvas.getTotalDrbNumber()).divide(new BigDecimal(blockNumber), 4, RoundingMode.FLOOR)
                                .multiply(canvasDrbStatistics.getMintRevenue());
                canvasDrbStatistics.setDre(dre);

                canvasDrbStatistics.setCanvasName(canvas.getCanvasName());
                canvasDrbStatistics.setCanvasLogo(canvas.getCanvasLogo());
                canvasDrbStatistics.setCanvasDescription(canvas.getCanvasDescription());
                canvasDrbStatistics.setCanvasNumber(canvas.getCanvasNumber() + "");
                canvasDrbStatistics.setDaoNumber(canvas.getDaoNumber() + "");

                Integer cOwnerAmount = workService.selectNftOwnersByCanvasId(canvas.getCanvasId());
                Integer cNftAmount = workService.selectNftAmountsByCanvasId(canvas.getCanvasId());
                Integer cWorkAmount = workService.selectWorkAmountsByCanvasId(canvas.getCanvasId());
                canvasDrbStatistics.setOwners(cOwnerAmount);
                canvasDrbStatistics.setNft(cNftAmount);
                canvasDrbStatistics.setWorkAmount(cWorkAmount);

                canvasDrbStatistics.setStatus(StatisticsStatusEnum.JSWC.getStatus());

                canvas.setRestDrb(drbNumber);
                // 更新最新价格的drb
                log.info("[handleCanvasDrbStatistics]drbNumber:{} canvasDrbStatistics:{} canvas:{}", drbNumber, JacksonUtil.obj2json(canvasDrbStatistics), JacksonUtil.obj2json(canvas));
                int i = canvasDrbStatisticsService.updateCanvasAndCanvasDrbStatistics(canvasDrbStatistics, canvas);
                log.info("[handleCanvasDrbStatistics]drbNumber:{}  canvasId:{} i:{}", drbNumber, canvas.getId(), i);

            } catch (Exception e) {
                log.error("[handleCanvasDrbStatistics]drbNumber:{}  canvasId:{} e:", drbNumber, canvasDrbStatistics.getCanvasId(), e);
            }
        }
    }

    /**
     * 新建用户
     *
     * @param userAddress 用户地址
     * @return User
     */
    public User newUser(String userAddress) {
        User user = new User();
        user.setUserAddress(CommonUtil.addHexPrefixIfNotExist(userAddress));
        Random random = new Random();
        int i = random.nextInt(32) + 1;
        String avatar = String.format(headImage, i);
        user.setAvatarAddress(avatar);
        user.setFirstLoginTime(LocalDateTime.now());
        user.setSignPrivacyAgreement(SignPrivacyEnum.WQS.getCode());
        Result<Boolean> resultBoolean = iSubscriptionService.ethGetCodeCheckUserAddress(ProtoDaoConstant.netWork,
                CommonUtil.addHexPrefixIfNotExist(userAddress));
        if (resultBoolean != null && resultBoolean.getData()) {
            user.setIsContract(0);
        } else {
            user.setIsContract(1);
        }
        return user;
    }

    public void updateCanvasPrice(Integer drbNumber, Integer daoId) {

        List<Canvas> canvasList = canvasService.listCanvasLessThanDrb(drbNumber);
        if (daoId != null) {
            canvasList = canvasList.stream().filter(v -> v.getDaoId().equals(daoId)).collect(Collectors.toList());
        }
        Map<Integer, Dao> daoMap = new HashMap<>();
        for (Canvas canvas : canvasList) {
            try {
                Dao dao = daoMap.get(canvas.getDaoId());
                if (dao == null) {
                    dao = daoService.getById(canvas.getDaoId());
                    daoMap.put(canvas.getDaoId(), dao);
                }
                if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                    if (daoId == null) {
                        canvas.setRestDrb(drbNumber);
                    }
                    canvas.setCurrentPrice(dao.getGlobalDaoPrice());
                    canvasService.updateById(canvas);
                    continue;
                }
                // canvas next price
                InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                infuraCallRequestDto.setTo(ContractMethodEnum.CANVAS_NEXT_PRICE.getContractAddress());
                infuraCallRequestDto
                        .setData(ContractMethodEnum.CANVAS_NEXT_PRICE.getMethodAddress() + canvas.getCanvasId());

                // 调用查询使用数据集的user
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[updateCanvasPrice] error result:{} canvasId:{}", result.getResultDesc(),
                            canvas.getId());
                    // throw new RuntimeException("保存canvas查询价格信息失败");
                    continue;
                }
                // log.info("[updateCanvasPrice]infura return data:{}", result.getData());
                String canvasInfoData = result.getData();
                String price = CommonUtil.hexToTenString(canvasInfoData);
                log.info("[updateCanvasPrice]cavasId:{}, price:{}", canvas.getId(), price);
                if (StringUtils.isNotBlank(price)) {
                    BigDecimal canvasPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()));
                    if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
                        canvasPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()));
                    }

                    if (canvasPrice.compareTo(canvas.getCurrentPrice()) != 0) {
                        log.info("[updateCanvasPrice] price is changed cavasId:{} originPrice:{} price:{}",
                                canvas.getId(), canvas.getCurrentPrice(), canvasPrice);
                    }
                    if (!canvas.getCurrentPrice().equals(canvasPrice)) {
                        log.info("[updateCanvasPrice]cavasId:{}, oldPrice:{} newPrice:{}", canvas.getId(), canvas.getCurrentPrice(), canvasPrice);
                    }
                    canvas.setCurrentPrice(canvasPrice);
                }
                if (daoId == null) {
                    canvas.setRestDrb(drbNumber);
                }
                canvasService.updateById(canvas);
            } catch (Exception e) {
                log.error("[updateCanvasPrice] canvasId:{} error:", canvas.getId(), e);
            }

        }
    }

    /**
     * 更新dao价格
     */
    public void updateDaoPrice() {

        List<Dao> daoList = daoService.daoStarted();
        for (Dao dao : daoList) {
            Canvas canvas = canvasService.listCanvasFloorPriceByDaoId(dao.getId() + "");
            if (canvas != null && canvas.getCurrentPrice() != null) {
                log.info("[updateDaoPrice] daoId:{} price:{}", dao.getId(), canvas.getCurrentPrice());
                if (canvas.getCurrentPrice().compareTo(dao.getCanvasFloorPrice()) != 0) {
                    log.info("[updateDaoPrice] price is changed daoId:{} originPrice:{} price:{}", dao.getId(),
                            dao.getCanvasFloorPrice(), canvas.getCurrentPrice());
                    dao.setCanvasFloorPrice(canvas.getCurrentPrice());
                    if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                        dao.setCanvasFloorPrice(dao.getGlobalDaoPrice());
                    }
                    daoService.updateById(dao);
                }
            }
        }
    }

    /**
     * 关闭dao
     *
     * @param drbNumber drb
     */
    public void closeDao(Integer drbNumber) {
        List<Dao> daoList = daoService.selectEndedDaoByDrb(drbNumber);
        for (Dao dao : daoList) {
            IPage<DaoDrbStatistics> page = new Page<>(1, 10);
            Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(page, dao.getId());
            if (daoDrbStatisticsPage.getTotal() >= dao.getDaoMintWindow()) {
                dao.setDaoStatus(DaoStatusEnum.FINISHED.getStatus());
                daoService.updateById(dao);

                List<Canvas> canvasList = canvasService.listCanvasByDaoId(dao.getId() + "");
                if (!canvasList.isEmpty()) {
                    canvasList.forEach(v -> v.setDaoStatus(DaoStatusEnum.FINISHED.getStatus()));
                    canvasService.updateBatchById(canvasList);
                }

                List<Work> workList =
                        workService.selectWorksByDaoIdAndStatus(dao.getId() + "", WorkStatusEnum.NOT_CAST.getStatus());
                if (!workList.isEmpty()) {
                    workList.forEach(v -> {
                        v.setWorkStatus(WorkStatusEnum.EXPIRED.getStatus());
                        v.setDaoStatus(DaoStatusEnum.FINISHED.getStatus());
                    });
                    workService.updateBatchById(workList);
                }
            }
        }
    }

    /**
     * 1.4 去掉这里的逻辑，不需要监听drb变化了，因为dao的drb和这个值没有意义了 开启dao
     *
     * @param drbNumber drb
     */
    @Transactional
    public void startDao(Integer drbNumber) {
        List<Dao> daoList = daoService.selectStartDaoByDrb(drbNumber);
        if (!daoList.isEmpty()) {
            List<Dao> daos = new ArrayList<>();
            daoList.forEach(v -> {
                if (!"0".equals(this.getDaoCurrentRound(v.getProjectId()))) {
                    Dao updateDao = new Dao();
                    updateDao.setId(v.getId());
                    updateDao.setDaoStatus(DaoStatusEnum.STARTED.getStatus());
                    updateDao.setSyncDex(1);
                    daos.add(updateDao);
                }
            });
            if (!daos.isEmpty()) {
                daoService.updateBatchById(daos);
            }
        }

    }

    /**
     * 代币发放逻辑是1-衰减曲线的生成DaodrbStatistics
     *
     * @param drbNumber drb
     */
    public void createCurrentDrbDaoStatistics(Integer drbNumber) {
        List<Dao> daoList = daoService.selectGenerationMethodDao();
        for (Dao dao : daoList) {
            try {
                DaoDrbStatistics daoDrbStatistics1 = new DaoDrbStatistics();
                daoDrbStatistics1.setFloorPrice(dao.getDaoFloorPrice());
                if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                    daoDrbStatistics1.setFloorPrice(dao.getGlobalDaoPrice());
                }
                daoDrbStatistics1.setDaoId(dao.getId());
                daoDrbStatistics1.setStatus(StatisticsStatusEnum.WJS.getStatus());
                daoDrbStatistics1.setDrbVol(BigDecimal.ZERO);
                daoDrbStatistics1.setDrbVolExTax(BigDecimal.ZERO);

                DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
                if (daoDrbStatistics != null) {
                    daoDrbStatistics1.setFloorPrice(daoDrbStatistics.getFloorPrice());
                    daoDrbStatistics1.setCanvas(daoDrbStatistics.getCanvas());
                    daoDrbStatistics1.setOwners(daoDrbStatistics.getOwners());
                    daoDrbStatistics1.setNft(daoDrbStatistics.getNft());
                    daoDrbStatistics1.setWorks(daoDrbStatistics.getWorks());
                    daoDrbStatistics1.setMintRevenue(daoDrbStatistics.getMintRevenue());
                    daoDrbStatistics1.setMintRevenueExTax(daoDrbStatistics.getMintRevenueExTax());
                    if (daoDrbStatistics.getDaoReward() != null) {
                        daoDrbStatistics1.setDaoReward(daoDrbStatistics.getDaoReward());
                    }
                    if (daoDrbStatistics.getDaoAssetPool() != null) {
                        daoDrbStatistics1.setDaoAssetPool(daoDrbStatistics.getDaoAssetPool());
                    }
                    if (daoDrbStatistics.getMintRevenue() != null) {
                        daoDrbStatistics1.setMintRevenue(daoDrbStatistics.getMintRevenue());
                    }
                    if (daoDrbStatistics.getMintRevenueExTax() != null) {
                        daoDrbStatistics1.setMintRevenueExTax(daoDrbStatistics.getMintRevenueExTax());
                    }
                    //计算dre
                    if (daoDrbStatistics.getMintRevenue() != null) {
                        Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
                        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
                        long blockNumber = daoDrbStatisticsPage.getTotal() + 1;
                        BigDecimal dre = new BigDecimal(dao.getDaoMintWindow())
                                .divide(new BigDecimal(blockNumber), 4, RoundingMode.FLOOR).multiply(daoDrbStatistics.getMintRevenue());
                        daoDrbStatistics1.setDre(dre.setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString());
                    }

                    //计算SevenDayDrbVol
                    Integer startDrb = drbNumber >= 6 ? drbNumber - 6 : 0;
                    List<DaoDrbStatistics> daoDrbStatisticsList1 = daoDrbStatisticsService.selectGalleryDao(startDrb, drbNumber);
                    BigDecimal sevenDayDrbVol = daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(dao.getId()))
                            .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                    daoDrbStatistics1.setSevenDayDrbVol(sevenDayDrbVol);
                }

                daoDrbStatistics1.setDrbNumber(drbNumber);
                log.info("[createCurrentDrbDaoStatistics] createDrbStatistics daoId:{} drbNumber:{}", dao.getId(), drbNumber);
                daoDrbStatisticsService.save(daoDrbStatistics1);
            } catch (Exception e) {
                log.error("[createCurrentDrbDaoStatistics] exception createDrbStatistics daoId:{} drbNumber:{} e:", dao.getId(), drbNumber, e);
            }
        }
    }

    public void currentDrbDaoStatistics(Integer drbNumber) {
        // 1查询drb下所有dao统计信息
        // 2 遍历每一个dao，计算统计信息
        // 3 更新统计信息更新完成

        List<DaoDrbStatistics> daoDrbStatisticsList = daoDrbStatisticsService.selectByDrbNumber(drbNumber);
        if (!daoDrbStatisticsList.isEmpty()) {
            daoDrbStatisticsList.forEach(v -> v.setStatus(StatisticsStatusEnum.JSZ.getStatus()));
            daoDrbStatisticsService.updateBatchById(daoDrbStatisticsList);

            this.handleDaoDrbStatistics(daoDrbStatisticsList, drbNumber);

        }

        this.currentDrbCanvasStatistics(drbNumber);

        this.closeDao(drbNumber + 1);
        this.startDao(drbNumber + 1);
        this.createCurrentDrbDaoStatistics(drbNumber + 1);
    }

    public void currentDrbCanvasStatistics(Integer drbNumber) {

        // 更新所有canvas的最近铸造价格
        this.updateCanvasPrice(drbNumber, null);

        List<CanvasDrbStatistics> canvasDrbStatisticsList = canvasDrbStatisticsService.selectByDrbNumber(drbNumber);
        if (!canvasDrbStatisticsList.isEmpty()) {
            canvasDrbStatisticsList.forEach(v -> v.setStatus(StatisticsStatusEnum.JSZ.getStatus()));
            canvasDrbStatisticsService.updateBatchById(canvasDrbStatisticsList);

            this.handleCanvasDrbStatistics(canvasDrbStatisticsList, drbNumber);
            // 计算当前drb铸造收益
            this.handleCurrentDrbMinterProfit(canvasDrbStatisticsList, drbNumber);
        }

        // 更新dao最新价格
        this.updateDaoPrice();

    }

    public void handleCurrentDrbMinterProfit(List<CanvasDrbStatistics> canvasDrbStatisticsList, Integer drbNumber) {
        try {
            if (canvasDrbStatisticsList == null || canvasDrbStatisticsList.size() == 0) {
                return;
            }
            log.info("[handleCurrentDrbMinterProfit] canvasDrbStatisticsList size:{} drbNumber:{}",
                    canvasDrbStatisticsList.size(), drbNumber);
            List<Subscribe> subscribeList = subscribeService.selectByTradeType(TradeTypeEnum.CURRENT_ROUND.getType());
            Result<String> result = iSubscriptionService.subscripeHeight(subscribeList.get(0).getFilterId());
            log.info("[handleCurrentDrbMinterProfit] subscripeHeight resultData:{}",
                    JacksonUtil.obj2json(result.getData()));
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[handleCurrentDrbMinterProfit] search blockNo error filterId:{} err:{}",
                        subscribeList.get(0).getFilterId(), result.getResultDesc());
                return;
            }
            String blockNo = result.getData();
            List<Integer> daoIdList =
                    canvasDrbStatisticsList.stream().map(CanvasDrbStatistics::getDaoId).collect(Collectors.toList());
            List<Dao> daoList = daoService.selectDaoByIds(daoIdList);
            Map<Integer, Dao> daoMap = daoList.stream().collect(Collectors.toMap(Dao::getId, v -> v));
            // key为userAddress+dao projectId
            Map<String, Integer> userMap = new HashMap<>();
            for (CanvasDrbStatistics canvasDrbStatistics : canvasDrbStatisticsList) {
                try {
                    Dao dao = daoMap.get(canvasDrbStatistics.getDaoId());
                    if (dao.getDaoVersion().equals(1)) {
                        continue;
                    }
                    //1.3去掉了，不需要查询分配比例，只要是铸造过work的都查询一遍
//                    // subscribe/heigh 通过这个方法查询blockheight区块，
//                    InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
//                    infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
//                    infuraCallRequestDto.setTo(ContractMethodEnum.getNftMinterERC20Ratio.getContractAddress());
//                    infuraCallRequestDto
//                            .setData(ContractMethodEnum.getNftMinterERC20Ratio.getMethodAddress() + dao.getProjectId());
//                    // 待测试 blockNo
//                    if (StringUtils.isNotBlank(blockNo)) {
//                        infuraCallRequestDto.setBlockNumber(blockNo);
//                    }
//                    // 调用查询使用数据集的user
//                    log.info("[handleCurrentDrbMinterProfit] infuraCallRequestDto:{}",
//                            JacksonUtil.obj2json(infuraCallRequestDto));
//                    result = iSubscriptionService.infuraCall(infuraCallRequestDto);
//                    log.info("[handleCurrentDrbMinterProfit] getNftMinterERC20Ratio daoId:{} resultData:{}", dao.getId(),
//                            JacksonUtil.obj2json(result.getData()));
//                    if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                        log.error("[handleCurrentDrbMinterProfit] getNftMinterERC20Ratio error result:{} daoId:{}",
//                                result.getResultDesc(), dao.getId());
//                        continue;
//                    }
//                    String data = result.getData();
//                    String price = CommonUtil.hexToTenString(data);
//                    log.info("[handleCurrentDrbMinterProfit]getNftMinterERC20Ratio daoId:{} infura return data:{} price:{}",
//                            dao.getId(), result.getData(), price);
//                    if (StringUtils.isNotBlank(price) && !"0".equals(price)) {
                    List<Work> workList =
                            workService.selectDrbNftByCanvasId(canvasDrbStatistics.getCanvasId() + "", drbNumber + "");
                    if (!workList.isEmpty()) {
                        Map<String, List<Work>> workMap =
                                workList.stream().collect(Collectors.groupingBy(Work::getMintedAddress));
                        for (String userAddress : workMap.keySet()) {
                            if (userMap.get(userAddress + dao.getProjectId()) != null) {
                                log.info(
                                        "[handleCurrentDrbMinterProfit] has already been calculated daoId:{} userAddress:{} ",
                                        dao.getId(), userAddress);
                                continue;
                            }
                            // 查询Minter可领取代币数量
                            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                            infuraCallRequestDto.setTo(ContractMethodEnum.CLAIM_NFT_MINTER_REWARD_FUNDING.getContractAddress());
                            infuraCallRequestDto.setFrom(userAddress);
                            infuraCallRequestDto.setData(ContractMethodEnum.CLAIM_NFT_MINTER_REWARD_FUNDING.getMethodAddress()
                                    + dao.getProjectId() + CommonUtil
                                    .fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(userAddress)));
                            infuraCallRequestDto.setBlockNumber(null);
                            // 调用查询使用数据集的user
                            result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                            log.info(
                                    "[handleCurrentDrbMinterProfit]claimNftMinterReward daoId:{} infura return data:{} ",
                                    dao.getId(), result.getData());
                            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                                log.error(
                                        "[handleCurrentDrbMinterProfit] claimNftMinterReward error result:{} daoId:{}",
                                        result.getResultDesc(), dao.getId());
                                continue;
                            }
                            List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
                            String nftMinterERC20Reward = CommonUtil.hexToTenString(dataList.get(0));
                            String nftMinterETHReward = CommonUtil.hexToTenString(dataList.get(1));
//                                String claimMinterReward = CommonUtil.hexToTenString(result.getData());
//                                if (StringUtils.isNotBlank(claimMinterReward) && !"0".equals(claimMinterReward)) {
                            UserHarvestToken userHarvestToken = userHarvestTokenService
                                    .selectByDaoIdAndUserAddress(canvasDrbStatistics.getDaoId(), userAddress);
                            if (StringUtils.isNotBlank(nftMinterERC20Reward) && new BigDecimal(nftMinterERC20Reward).compareTo(BigDecimal.ZERO) > 0) {

                                if (userHarvestToken == null) {
                                    userHarvestToken = new UserHarvestToken();
                                    userHarvestToken.setDaoId(dao.getId());
                                    userHarvestToken.setCanvasId(canvasDrbStatistics.getCanvasId());
                                    userHarvestToken.setUserAddress(userAddress);
                                }
                                BigDecimal unclaimedToken = new BigDecimal(nftMinterERC20Reward)
                                        .divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR);
                                userHarvestToken.setUnclaimedToken(unclaimedToken);

                                userMap.put(userAddress + dao.getProjectId(), dao.getId());

                            }
                            if (StringUtils.isNotBlank(nftMinterETHReward) && new BigDecimal(nftMinterETHReward).compareTo(BigDecimal.ZERO) > 0) {

                                if (userHarvestToken == null) {
                                    userHarvestToken = new UserHarvestToken();
                                    userHarvestToken.setDaoId(dao.getId());
                                    userHarvestToken.setCanvasId(canvasDrbStatistics.getCanvasId());
                                    userHarvestToken.setUserAddress(userAddress);
                                }
                                BigDecimal unclaimedToken = new BigDecimal(nftMinterETHReward)
                                        .divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);
                                userHarvestToken.setUnclaimedEth(unclaimedToken);

                                userMap.put(userAddress + dao.getProjectId(), dao.getId());
                            }
                            if (userHarvestToken != null) {
                                log.info("[handleCurrentDrbMinterProfit]UserHarvestToken daoId:{} userAddress:{} userHarvestToken:{}",
                                        dao.getId(), userAddress, JacksonUtil.obj2json(userHarvestToken));
                                userHarvestTokenService.saveOrUpdate(userHarvestToken);
                            }
                        }
                    }
//                    }
                } catch (Exception e) {
                    log.error("[handleCurrentDrbMinterProfit]UserHarvestToken error canvasId:{} e:", canvasDrbStatistics.getCanvasId(), e);
                }
            }
        } catch (Exception e) {
            log.error("[handleCurrentDrbMinterProfit] exception drbNumber:{} e:", drbNumber, e);
        }

    }

    /**
     * 根据当前DRB 计算下一个DRB开始的区块高度，并赋值给Dao4ArtConstant.NEXT_DRB_START_BLOCK
     *
     * @param drbNumber
     */
    public void handleNextDrbStartBlock(Integer drbNumber) {

        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.NEXT_DRB_START_BLOCK.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.NEXT_DRB_START_BLOCK.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(drbNumber + 1)));

            // 调用查询使用数据集的user
            Result<String> startBlockResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (startBlockResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[handleNextDrbStartBlock]infura nextDrbStartBlock return data:{}", startBlockResult.getData());
                String startBlockData = startBlockResult.getData();
                String startBlock = CommonUtil.hexToTenString(startBlockData);
                log.info("[handleNextDrbStartBlock]infura nextDrbStartBlock startBlock:{}", startBlock);
                if (StringUtils.isNotBlank(startBlock)) {
                    ProtoDaoConstant.NEXT_DRB_START_BLOCK = Integer.valueOf(startBlock);
                }
            } else {
                log.error("[handleDaoDrbStatistics]infura PROJECT_CLAIM return data:{} desc:{} ", startBlockResult.getData(), startBlockResult.getResultDesc());
            }
        } catch (Exception e) {
            log.error("[handleNextDrbStartBlock]infura nextDrbStartBlock error:", e);
        }

    }

    public void updateCanvasPrice(Dao dao) {
        List<Canvas> canvasList = canvasService.listCanvasByDaoId(dao.getId() + "");
        for (Canvas canvas : canvasList) {
            try {
                if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                    canvas.setCurrentPrice(dao.getGlobalDaoPrice());
                    canvasService.updateById(canvas);
                    continue;
                }
                // canvas next price
                InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                infuraCallRequestDto.setTo(ContractMethodEnum.CANVAS_NEXT_PRICE.getContractAddress());
                infuraCallRequestDto
                        .setData(ContractMethodEnum.CANVAS_NEXT_PRICE.getMethodAddress() + canvas.getCanvasId());

                // 调用查询使用数据集的user
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[CommonService]-updateCanvasPrice error result:{} canvasId:{}", result.getResultDesc(),
                            canvas.getId());
                    // throw new RuntimeException("保存canvas查询价格信息失败");
                    continue;
                }
                String canvasInfoData = result.getData();
                String price = CommonUtil.hexToTenString(canvasInfoData);
                log.info("[CommonService]-updateCanvasPricecavasId:{}, price:{}", canvas.getId(), price);
                if (StringUtils.isNotBlank(price)) {
                    BigDecimal canvasPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()));   // dao.getInputTokenDecimals()
                    if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
                        canvasPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()));
                    }
                    if (canvasPrice.compareTo(canvas.getCurrentPrice()) != 0) {
                        log.info("[CommonService]-updateCanvasPrice price is changed cavasId:{} originPrice:{} price:{}",
                                canvas.getId(), canvas.getCurrentPrice(), canvasPrice);
                    }
                    canvas.setCurrentPrice(canvasPrice);
                }
                canvasService.updateById(canvas);
            } catch (Exception e) {
                log.error("[CommonService]-updateCanvasPrice canvasId:{} error:", canvas.getId(), e);
            }

        }

    }

    //要不要在canvas页面去做work的生成呢
    public boolean addWork(Dao dao, Canvas canvas, Integer workNumber) {
        // dao 的name和编号
        //1. 获取原图片地址 保存到本地
        //2。生成图片
        // 3. 上传到aws上
        //4.上传workUri地址
        //5.保存一条work信息addWork

        // 固定为一口价 价格为0。01


        try {

            //上传json文件
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            //上传 图片
//            String imageFileName = 0 + dao.getWorkUrlSuffix();
            String imageFileNameAws = workNumber + dao.getWorkUrlSuffix();
            String dirStr = String.format(workImageUrl, dao.getDaoNumber());
            File dirPath = new File(dirStr);
            if (!dirPath.exists()) {
                dirPath.mkdirs();
            }
            String filePath = dirStr + File.separator + imageFileNameAws;
            File imageFile = new File(filePath);
            //图片添加文字
            String workImageDefaultUrl = ProtoDaoConstant.workImageDefaultUrl;
            if (StringUtils.isNotBlank(dao.getDaoLogoUrl()) && !dao.getDaoLogoUrl().equals(daoDefaultLogo)) {
                workImageDefaultUrl = String.format(ProtoDaoConstant.workImageDaoLogoUrl, dao.getDaoNumber()) + File.separatorChar + dao.getDaoNumber() + dao.getWorkUrlSuffix();
            }
            // 如果dao的work url 为默认，，生成pass卡。。且图片上有数字--->当dao图片修改，这个图片需要修改
            // 如果不为默认，需要用dao的work url，图片上没有数字--->当dao图片修改，这个图片需要修改
            // TODO:直接用dao 的图片？没哟与数字..，不好区分
            // TODO:生成，在更新dao的时候不用修改?
            // 如果pass卡图片被修改，在更新dao图片的时候就不用修改了
            // 生成时候的问题，，修改时候的问题..

            // TODO：如果不做调整，修改dao之后，图片为什么被替换掉
            // TODO：如果不做调整，修改dao之后，如果判断图片是否被修改？

            String workName = "work" + workNumber;
//            String workName = workNumber == null ? null : "work" + workNumber;
            ImageUtil.imageAddText(workImageDefaultUrl, filePath, dao.getDaoName(), workName, dao.getWorkUrlSuffix().substring(1));

            String workHash = ImageUtil.getMD5(imageFile);

            Work workNum = workService.selectWorkByNumber(dao.getId(), String.valueOf(workNumber));
            Work work = new Work();
            if (workNum != null) {
                work.setId(workNum.getId());
            }
            ImageUtil.HeightAndBgColor heightAndBgColor = ImageUtil.getImageRgb(imageFile);
            if (heightAndBgColor != null) {
                log.info("daoNumber:{} workNumber:{} height:{} bgColor:{}", dao.getDaoNumber(), workNumber, heightAndBgColor.getHeight(),
                        heightAndBgColor.getBgColor());
                work.setHeight(heightAndBgColor.getHeight());
                work.setBgColor(heightAndBgColor.getBgColor());
            } else {
                log.error("daoNumber:{} workNumber:{} image not height error", dao.getDaoNumber(), workNumber);
                // work.setHeight(260.0);
                // work.setBgColor("#FFFFFF");
                return false;
            }

            String imageUrl = "";

            try {
                // 文件上传前的名称 处理图片用
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.workBucketName + "/" + dao.getDaoNumber(), imageFile, imageFileNameAws, true);

                imageUrl = urlPrefix + ProtoDaoConstant.workBucketName + "/" + dao.getDaoNumber() + "/" + imageFileNameAws;
                log.info("[work-create] s3DaoLogoUrl:{}", imageUrl);
            } catch (Exception e) {
                log.error("[work-create]i:{} upload image error", workNumber, e);
                return false;
            }

            // 处理uri用
            WorkCreateReqVo workCreateReqVo = new WorkCreateReqVo();
            workCreateReqVo.setImageUrl(imageUrl);
            MintWorkUriDto mintWorkUriDto = MintWorkUriDto.transfer(workCreateReqVo);
            String imageName = dao.getDaoNumber() + "-" + workNumber + "";
            String fileName = imageName + ".json";
            String s3FileName =
                    urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName + "/" + fileName;
            try {
                BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
                representaion.setObjectName(fileName);
                representaion.setText(JacksonUtil.obj2json(mintWorkUriDto));
                s3Service.putObject(
                        ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName,
                        representaion);
                log.info("[work-create] s3FileName:{}", s3FileName);
            } catch (Exception e) {
                log.error("[work-create]i:{} upload image error", workNumber, e);
                return false;
            }

            if (workNum == null) {

                work.setWorkDescription(workCreateReqVo.getWorkDescription());
//                work.setCreatorAddress(workCreateReqVo.getUserAddress());
                work.setWorkStatus(WorkStatusEnum.NOT_CAST.getStatus());
                work.setCreateTime(LocalDateTime.now());
                work.setDaoId(dao.getId());
                if (canvas != null) {
                    work.setCanvasId(CommonUtil.removeHexPrefixIfExists(canvas.getCanvasId()));
                    work.setCanId(canvas.getId());
                    work.setCanvasNumber(canvas.getCanvasNumber());
                }
                work.setImageUrl(imageUrl);
                work.setDaoNumber(dao.getDaoNumber());
                work.setProjectId(dao.getProjectId());
                work.setWorkHash(workHash);
                work.setWorkUri(s3FileName);
                work.setFavoriteAmount(0);
                work.setWorkNumber(workNumber);
                //空串
                work.setCreateSignHash("");
                work.setCreatorAddress(dao.getOwnerAddress());
                work.setOwnerAddress(dao.getOwnerAddress());
                work.setGenerate(1);
                // 一口价 0.01
                work.setPriceType(WorkPriceTypeEnum.FIXED_PRICE.getType());
                if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                    work.setFixedPrice(dao.getGlobalDaoPrice());
                } else {
                    work.setFixedPrice(dao.getDaoFloorPrice());
                }

                workService.save(work);
            } else {
                work.setImageUrl(imageUrl);
                workService.updateById(work);
            }
            return true;

        } catch (Exception e) {
            log.error("daoNumber:{} workNumber:{} exception:", dao.getDaoNumber(), workNumber, e);
            return false;
        }
    }


//    public void updateTopupModeMinterHarvest(Integer drb) {
//        List<Work> workList = workService.selectTopupWorkForCal(drb);
//        for (Work work : workList) {
//            Dao dao = daoService.getById(work.getDaoId());
//            String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
//            updateMinterTopupHarvest(projectId, work.getMintedAddress());
//
//        }
//    }

    public Double nextDrbStartTime() {
        JSONObject jsonObject = ProtoDaoCommonUtil.blockTime(String.valueOf(ProtoDaoConstant.NEXT_DRB_START_BLOCK));
        if (jsonObject == null) {
            log.error("[drbInfo] blockTime jsonObject is null");
            return null;
        }
        Double nextPrbStartTime = jsonObject.getDouble("EstimateTimeInSec");
        return nextPrbStartTime;
    }

    public void updateMinterWorkTopupHarvest(String projectId, Integer workId, Dao mountDao, Work mountWork) {
        // dao_id-->dao.projectID
        // address erc721Address-->dao.erc721token
        // uint256 tokenId-->work.workNumber
        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.UPDATE_WORK_TOP_UP_ACCOUNT.getContractAddress());
            infuraCallRequestDto.setFrom(ProtoDaoConstant.ZERO_ADDRESS); // 已确认，任何地址都可以调
            infuraCallRequestDto.setData(ContractMethodEnum.UPDATE_WORK_TOP_UP_ACCOUNT.getMethodAddress()
                    + projectId
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(mountDao.getErc721Token()))
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(CommonUtil.tenToHex(mountWork.getWorkNumber()))) // token需要转换为16禁止
            );
            infuraCallRequestDto.setBlockNumber(null);

            log.info("[updateMinterWorkTopupHarvest]调用获取钱包余额的参数:" + JacksonUtil.obj2json(infuraCallRequestDto));
            Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[updateMinterWorkTopupHarvest] 调用获取钱包余额error result:{},调用的参数为:{}", result.getData(), JacksonUtil.obj2json(infuraCallRequestDto));
                return;
            }
            List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
            String erc20Amount = CommonUtil.hexToTenString(dataList.get(0));
            String ethAmount = CommonUtil.hexToTenString(dataList.get(1));
            log.info("插入--获取到的余额erc20Amount:" + erc20Amount);
            log.info("插入--获取到的余额ethAmount:" + ethAmount);

            WorkTopupHarvest workTopupHarvest = workTopupHarvestService.selectByProjectIdAndMountWorkId(projectId, mountWork.getId());
            Dao dao = daoService.daoDetailByProjectId(projectId);
            if (workTopupHarvest == null) {
                workTopupHarvest = new WorkTopupHarvest();

                workTopupHarvest.setWorkId(workId);
                workTopupHarvest.setDaoId(dao.getId());
                workTopupHarvest.setProjectId(projectId);

                workTopupHarvest.setMountWorkId(mountWork.getId());
                workTopupHarvest.setMountWorkNumber(mountWork.getWorkNumber());
                workTopupHarvest.setMountErc721Address(mountDao.getErc721Token());
            }
            // 需要判断是否是外部erc..，只有erc20需要除,eth一直除18
            if (dao.getIsThirdpartyToken().equals(1)) {
                // 如果绑定的dao是外部的erc20token
                String decimals = erc20Decimals(dao.getErc20Token());
                if (StringUtils.isBlank(decimals)) {
                    log.error("[updateMinterWorkTopupHarvest] mountDao:{} getDecimals error", mountDao.getId());
                } else {
                    //workTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(new BigDecimal(10*Integer.parseInt(decimals)), 18, RoundingMode.FLOOR));
                    // workTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(new BigDecimal("10").pow(Integer.parseInt(decimals))));
                    workTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(new BigDecimal("10").pow(Integer.parseInt(decimals))));
                }
            } else {
                workTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR));
            }
            workTopupHarvest.setEthAmount(new BigDecimal(ethAmount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR));

            // 更新on chain信息
            WorkTopupHarvest onChainWorkTopupHarvest = updateOnChainWorkTopupHarvest(workTopupHarvest, dao);
            workTopupHarvest.setInputTokenAmount(onChainWorkTopupHarvest.getInputTokenAmount());
            workTopupHarvest.setOutputTokenAmount(onChainWorkTopupHarvest.getOutputTokenAmount());

            workTopupHarvestService.saveOrUpdate(workTopupHarvest);
            log.info("[updateMinterWorkTopupHarvest]userTopupHarvest:{}", JacksonUtil.obj2json(workTopupHarvest));
        } catch (Exception e) {
            log.error("[updateMinterWorkTopupHarvest]error project:{} mount721Address:{}  mountWorkNumber:{} e",
                    projectId, mountDao.getErc721Token(), mountWork.getWorkNumber(), e);
        }

    }

    public void updateMinterWorkTopupHarvest(String projectId, Integer mountWorkId) {
        try {
            WorkTopupHarvest workTopupHarvest = workTopupHarvestService.selectByProjectIdAndMountWorkId(projectId, mountWorkId);
            Dao dao = daoService.daoDetailByProjectId(projectId);
            if (workTopupHarvest != null) {
                InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
                infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
                infuraCallRequestDto.setTo(ContractMethodEnum.UPDATE_WORK_TOP_UP_ACCOUNT.getContractAddress());
                infuraCallRequestDto.setFrom(ProtoDaoConstant.ZERO_ADDRESS); // 已确认，任何地址都可以调
                infuraCallRequestDto.setData(ContractMethodEnum.UPDATE_WORK_TOP_UP_ACCOUNT.getMethodAddress()
                        + projectId
                        + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(workTopupHarvest.getMountErc721Address()))
                        + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(CommonUtil.tenToHex(workTopupHarvest.getMountWorkNumber())))  // token需要转换为16禁止
                );
                infuraCallRequestDto.setBlockNumber(null);

                log.info("[updateMinterWorkTopupHarvest]调用获取钱包余额的参数:" + JacksonUtil.obj2json(infuraCallRequestDto));
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    log.error("[updateMinterWorkTopupHarvest] 调用获取钱包余额error result:{},调用的参数为:{}", result.getData(), JacksonUtil.obj2json(infuraCallRequestDto));
                    return;
                }
                List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
                String erc20Amount = CommonUtil.hexToTenString(dataList.get(0));
                String ethAmount = CommonUtil.hexToTenString(dataList.get(1));
                log.info("更新--获取到的余额erc20Amount:" + erc20Amount);
                log.info("更新--获取到的余额ethAmount:" + ethAmount);

                // 外部erc20和内部的余数不一致,只有erc20需要除,eth一直除18
                Dao mountDao = daoService.selectDaoByErc721Token(workTopupHarvest.getMountErc721Address());
                if (dao.getIsThirdpartyToken().equals(1) && dao.getErc20TokenDecimals() != null) {
                    // 如果绑定的dao是外部的erc20token
                    String decimals = erc20Decimals(dao.getErc20Token());
                    if (StringUtils.isBlank(decimals)) {
                        log.error("[updateMinterWorkTopupHarvest] mountDao:{} getDecimals error", dao.getId());
                    } else {
                        workTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(new BigDecimal("10").pow(Integer.parseInt(decimals))));
                    }
                } else {
                    workTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR));
                }
                workTopupHarvest.setEthAmount(new BigDecimal(ethAmount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR));

                // 更新on chain信息
                //WorkTopupHarvest onChainWorkTopupHarvest = updateOnChainWorkTopupHarvest(workTopupHarvest,dao);
                //workTopupHarvest.setInputTokenAmount(onChainWorkTopupHarvest.getInputTokenAmount());
                //workTopupHarvest.setOutputTokenAmount(onChainWorkTopupHarvest.getOutputTokenAmount());
                workTopupHarvestService.saveOrUpdate(workTopupHarvest);
                log.info("更新获取余额的WorkTopUP信息:" + JacksonUtil.obj2json(workTopupHarvest));
            } else {
                log.info("[updateMinterWorkTopupHarvest] have null work top-up: project:{},mountWorkId:{}", projectId, mountWorkId);
            }
        } catch (Exception e) {
            log.error("[updateMinterWorkTopupHarvest]error project:{},mountWorkId:{} e", projectId, mountWorkId, e);
        }

    }


    // 已废弃
//    public void updateMinterTopupHarvest(String projectId, String userAddress) {
//
//        try {
//            // 查询Minter获得的代币和eth数量  铸造的时候是实时更新，使用eth的时候是drb结束时更新
//            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
//            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
//            infuraCallRequestDto.setTo(ContractMethodEnum.UPDATE_TOP_UP_ACCOUNT.getContractAddress());
//            infuraCallRequestDto.setFrom(userAddress);
//            infuraCallRequestDto.setData(ContractMethodEnum.UPDATE_TOP_UP_ACCOUNT.getMethodAddress()
//                    + projectId + CommonUtil
//                    .fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(userAddress)));
//            infuraCallRequestDto.setBlockNumber(null);
//            // 调用查询使用数据集的user
//            Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
//            log.info("[updateMinterTopupHarvest]claimNftMinterRewardFunding daoId:{} infura return data:{} userAddress:{}",
//                    projectId, result.getData(), userAddress);
//            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                log.error("[updateMinterTopupHarvest] claimNftMinterRewardFunding error result:{} daoId:{}",
//                        result.getResultDesc(), projectId);
//                return;
//            }
//            List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
//            String erc20Amount = CommonUtil.hexToTenString(dataList.get(0));
//            String ethAmount = CommonUtil.hexToTenString(dataList.get(1));
//
//            UserTopupHarvest userTopupHarvest = userTopupHarvestService.selectByProjectIdAndUserAddress(projectId, userAddress);
//            if (userTopupHarvest == null) {
//                Dao dao = daoService.daoDetailByProjectId(projectId);
//                userTopupHarvest = new UserTopupHarvest();
//                userTopupHarvest.setDaoId(dao.getId());
//                userTopupHarvest.setProjectId(projectId);
//                userTopupHarvest.setSubdaoErc20Token(dao.getErc20Token());
//                userTopupHarvest.setUserAddress(userAddress);
//                userTopupHarvest.setRoundDrb(Integer.valueOf(dao.getCurrentRound()));
//            }
//            userTopupHarvest.setErc20Amount(new BigDecimal(erc20Amount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR));
//            userTopupHarvest.setEthAmount(new BigDecimal(ethAmount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR));
//
//            userTopupHarvestService.saveOrUpdate(userTopupHarvest);
//            log.info("[updateMinterTopupHarvest]userTopupHarvest:{}", JacksonUtil.obj2json(userTopupHarvest));
//
//        } catch (Exception e) {
//            log.error("[updateMinterTopupHarvest]error projectId:{} userAddress:{} e",
//                    projectId, userAddress, e);
//        }
//
//    }

    public WorkTopupHarvest updateOnChainWorkTopupHarvest(WorkTopupHarvest workTopupHarvest, Dao dao) throws Exception {
        // 更新work_topup_harvest表中的on chain...(input token,output token amount)
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.GET_TOP_UP_BALANCE.getContractAddress());
        infuraCallRequestDto.setFrom(ProtoDaoConstant.ZERO_ADDRESS); // 已确认，任何地址都可以调
        infuraCallRequestDto.setData(ContractMethodEnum.GET_TOP_UP_BALANCE.getMethodAddress()
                + workTopupHarvest.getProjectId()
                + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(workTopupHarvest.getMountErc721Address()))
                + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(CommonUtil.tenToHex(workTopupHarvest.getMountWorkNumber())))
        );

        log.info("[updateOnChainWorkTopupHarvest]调用获取钱包余额的参数:" + JacksonUtil.obj2json(infuraCallRequestDto));
        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[updateOnChainWorkTopupHarvest] 调用获取钱包余额error result:{},调用的参数为:{}", result.getData(), JacksonUtil.obj2json(infuraCallRequestDto));
            return null;
        }
        List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
        String ethAmount = CommonUtil.hexToTenString(dataList.get(0));
        String erc20Amount = CommonUtil.hexToTenString(dataList.get(1));
        log.info("[updateOnChainWorkTopupHarvest] 获取到的余额erc20Amount:{},获取到的余额ethAmount:{}", erc20Amount, ethAmount);

        // 外部erc20和内部的余数不一致,只有erc20需要除,eth一直除18
        if (dao.getIsThirdpartyToken().equals(1) && dao.getErc20TokenDecimals() != null) {
            // 如果绑定的dao是外部的erc20token
            String decimals = erc20Decimals(dao.getErc20Token());
            if (StringUtils.isBlank(decimals)) {
                log.error("[updateOnChainWorkTopupHarvest] mountDao:{} getDecimals error", dao.getId());
            } else {
                workTopupHarvest.setOutputTokenAmount(new BigDecimal(erc20Amount).divide(new BigDecimal("10").pow(Integer.parseInt(decimals))));
            }
        } else {
            workTopupHarvest.setOutputTokenAmount(new BigDecimal(erc20Amount).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()), 18, RoundingMode.FLOOR));
        }
        workTopupHarvest.setInputTokenAmount(new BigDecimal(ethAmount).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR));
        return workTopupHarvest;
    }

    public BigDecimal erc20BalanceOf(String erc20Address, String feePoolAddress, Integer bigdecimal, Integer inputTokenDecimal) {

        try {
            log.info("[erc20BalanceOf]erc20Address:{} feePoolAddress:{}", erc20Address, feePoolAddress);
            // 查询Minter获得的代币和eth数量  铸造的时候是实时更新，使用eth的时候是drb结束时更新
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(erc20Address);
            infuraCallRequestDto.setData(ContractMethodEnum.BALANCE_OF.getMethodAddress() + CommonUtil
                    .fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(feePoolAddress)));
            infuraCallRequestDto.setBlockNumber(null);
            // 调用查询使用数据集的user
            Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
            log.info("[erc20BalanceOf]erc20Address:{} feePoolAddress:{} resultData:{}", erc20Address, feePoolAddress, result.getData());
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[erc20BalanceOf]erc20Address:{} feePoolAddress:{} error result:{}", erc20Address, feePoolAddress, result.getResultDesc());
                return BigDecimal.ZERO;
            }
            List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
            String erc20Balance = CommonUtil.hexToTenString(dataList.get(0));

            log.info("[erc20BalanceOf]erc20Address:{} feePoolAddress:{} erc20Balance:{} ", erc20Address, feePoolAddress, erc20Balance);

            // 获取erc20Balance的decimal问题，不管是否开启erc20支付，都要除以output token的decimal
            String bigDecimalStr = bigdecimal == null ? ProtoDaoConstant.BASIC_RATIO : CommonUtil.getPowBigDecimal(bigdecimal).toPlainString();
            // String bigDecimalStr = bigdecimal == null ? CommonUtil.getPowBigDecimal(inputTokenDecimal).toPlainString() : CommonUtil.getPowBigDecimal(bigdecimal).toPlainString();
            return new BigDecimal(erc20Balance).divide(new BigDecimal(bigDecimalStr), 18, RoundingMode.FLOOR);

        } catch (Exception e) {
            log.error("[erc20BalanceOf]erc20Address:{} feePoolAddress:{} exception e:{}", erc20Address, feePoolAddress, e);
        }
        log.info("[erc20BalanceOf]erc20Address:{} feePoolAddress:{} return ZERO ", erc20Address, feePoolAddress);
        return BigDecimal.ZERO;

    }

    /**
     * 当canvas未创建时查询canvas价格的方法
     *
     * @param projectId
     * @param canvasId
     * @return
     */
    public BigDecimal getCanvasNextPrice(String projectId, String canvasId, Integer decimal, Integer inputTokenDecimal) {
        if (StringUtils.isAnyBlank(projectId, canvasId)) {
            log.info("[getCanvasNextPrice]projectId:{} or cavasId:{} is null", projectId, canvasId);
            return BigDecimal.ZERO;
        }
        // canvas next price
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.GET_CANVAS_NEXT_PRICE.getContractAddress());
        infuraCallRequestDto
                .setData(ContractMethodEnum.GET_CANVAS_NEXT_PRICE.getMethodAddress() + projectId + canvasId);

        // 调用查询使用数据集的user
        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[getCanvasNextPrice] error result:{} projectId:{} canvasId:{}", result.getResultDesc(), projectId, canvasId);
            return BigDecimal.ZERO;
        }
        String canvasInfoData = result.getData();
        String price = CommonUtil.hexToTenString(canvasInfoData);
        log.info("[getCanvasNextPrice]projectId:{} cavasId:{}, price:{}", projectId, canvasId, price);
        if (StringUtils.isNotBlank(price)) {
            BigDecimal canvasPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(inputTokenDecimal), 18, RoundingMode.HALF_UP);
            if (decimal != null) {
                // 如果decimal不为空，肯定开启了20支付
                // 但是如果开了20支付，decimal不为18，会出问题。
                canvasPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(decimal), 18, RoundingMode.HALF_UP);
            }

            return canvasPrice;
        }
        return BigDecimal.ZERO;
    }

    public BigDecimal ethBalanceOf(String ethAddress, Integer inputTokenDecimal) {

        try {
            log.info("[ethBalanceOf]ethAddress:{} ", ethAddress);
            if (StringUtils.isBlank(ethAddress)) {
                return BigDecimal.ZERO;
            }
            Result<String> result =
                    iSubscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(ethAddress));
            if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[ethBalanceOf]infura ethGetBalance return data:{} ", result.getData());
                String balance = result.getData();
                String price = CommonUtil.hexToTenString(balance);
                return new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(inputTokenDecimal), 18, RoundingMode.FLOOR);
            } else {
                log.error("[ethBalanceOf]infura ethGetBalance return data:{} ", result.getData());
            }


        } catch (Exception e) {
            log.error("[ethBalanceOf]ethAddress:{} exception e:{}", ethAddress, e);
        }
        return BigDecimal.ZERO;
    }

    public BigDecimal getRoundErc20Reward(Dao dao, Integer round) {

        try {
            // function getRoundERC20Reward(bytes32 daoId, uint256 round) public view returns (uint256)
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.getRoundERC20Reward.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.getRoundERC20Reward.getMethodAddress() + dao.getProjectId() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(round)));


            Result<String> getRewardTillRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (getRewardTillRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[getRoundERC20Reward]infura getRoundERC20Reward return data:{}", getRewardTillRoundResult.getData());
                String claim = getRewardTillRoundResult.getData();
                String claimPrice = CommonUtil.hexToTenString(claim);
                Integer pow = dao.getErc20TokenDecimals() == null ? 18 : dao.getErc20TokenDecimals();
                return new BigDecimal(claimPrice).divide(new BigDecimal("10").pow(pow));
            } else {
                log.error("[getRoundERC20Reward]infura getRoundERC20Reward return data:{} desc:{} daoId:{}",
                        getRewardTillRoundResult.getData(), getRewardTillRoundResult.getResultDesc(), dao.getId());
            }

        } catch (Exception e) {
            log.error("[getRoundERC20Reward]infura getRoundERC20Reward error daoId:{} round:{} e:{} ", dao.getId(), round, e);
            //如果失败了先置为0，之后修改数据库
        }
        return BigDecimal.ZERO;
    }

    public BigDecimal getRoundEthReward(Dao dao, Integer round) {

        try {
            // function getRoundERC20Reward(bytes32 daoId, uint256 round) public view returns (uint256)
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.getRoundETHReward.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.getRoundETHReward.getMethodAddress() + dao.getProjectId() + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(round)));


            Result<String> getRewardTillRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (getRewardTillRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[getRoundERC20Reward]infura getRoundERC20Reward return data:{}", getRewardTillRoundResult.getData());
                String claim = getRewardTillRoundResult.getData();
                String claimPrice = CommonUtil.hexToTenString(claim);
                Integer pow = dao.getErc20TokenDecimals() == null ? 18 : dao.getErc20TokenDecimals();
                return new BigDecimal(claimPrice).divide(new BigDecimal("10").pow(pow));
            } else {
                log.error("[getRoundERC20Reward]infura getRoundERC20Reward return data:{} desc:{} daoId:{}",
                        getRewardTillRoundResult.getData(), getRewardTillRoundResult.getResultDesc(), dao.getId());
            }

        } catch (Exception e) {
            log.error("[getRoundERC20Reward]infura getRoundERC20Reward error daoId:{} e:{} ", dao.getId(), e);
            //如果失败了先置为0，之后修改数据库
        }
        return BigDecimal.ZERO;
    }

    public BigDecimal getDaoRemainingRound(Dao dao) {

        try {
            // function getDaoRemainingRound(bytes32 daoId) public view returns (uint256)
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.getDaoRemainingRound.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.getDaoRemainingRound.getMethodAddress() + dao.getProjectId());


            Result<String> getRewardTillRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (getRewardTillRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[getRoundERC20Reward]infura getDaoRemainingRound return data:{}", getRewardTillRoundResult.getData());
                String claim = getRewardTillRoundResult.getData();
                String claimPrice = CommonUtil.hexToTenString(claim);
                return new BigDecimal(claimPrice);
            } else {
                // 如果开启了乐透，并且dao未开始
                // error = arithmetic underflow or overflow
                if (getRewardTillRoundResult.getResultDesc().contains("error = arithmetic underflow or overflow") && dao.getDaoStatus() == 1 && dao.getRoyaltyTokenLotteryMode() == 1) {
                    return BigDecimal.ZERO;
                }
                log.error("[getRoundERC20Reward]infura getDaoRemainingRound return data:{} desc:{} daoId:{}",
                        getRewardTillRoundResult.getData(), getRewardTillRoundResult.getResultDesc(), dao.getId());
            }

        } catch (Exception e) {
            log.error("[getRoundERC20Reward]infura getDaoRemainingRound error daoId:{} e:{} ", dao.getId(), e);
            //如果失败了先置为0，之后修改数据库
        }
        return BigDecimal.ZERO;
    }

    public BigDecimal getDaoLastActiveRound(Dao dao) {

        try {
            // function getDaoLastActiveRound(bytes32 daoId) public view returns (uint256)
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.getDaoLastActiveRound.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.getDaoLastActiveRound.getMethodAddress() + dao.getProjectId());


            Result<String> getRewardTillRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (getRewardTillRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[getRoundERC20Reward]infura getDaoLastActiveRound return data:{}", getRewardTillRoundResult.getData());
                String claim = getRewardTillRoundResult.getData();
                String claimPrice = CommonUtil.hexToTenString(claim);
                return new BigDecimal(claimPrice);
            } else {
                log.error("[getRoundERC20Reward]infura getDaoLastActiveRound return data:{} desc:{} daoId:{}",
                        getRewardTillRoundResult.getData(), getRewardTillRoundResult.getResultDesc(), dao.getId());
            }

        } catch (Exception e) {
            log.error("[getRoundERC20Reward]infura getDaoLastActiveRound error daoId:{} e:{} ", dao.getId(), e);
            //如果失败了先置为0，之后修改数据库
        }
        return new BigDecimal(dao.getCurrentRound());
    }

    public String erc20Decimals(String erc20Address) {

        try {
            log.info("[erc20Decimals]erc20Address:{} ", erc20Address);
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(erc20Address);
            infuraCallRequestDto.setData(ContractMethodEnum.DECIMALS.getMethodAddress());
            infuraCallRequestDto.setBlockNumber(null);
            // 调用查询使用数据集的user
            Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
            log.info("[erc20Decimals]erc20Address:{} resultData:{}", erc20Address, result.getData());
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[erc20Decimals]erc20Address:{} error result:{}", erc20Address, result.getResultDesc());
                return null;
            }
            List<String> dataList = CommonUtil.splitBy32Bytes(result.getData());
            String erc20Balance = CommonUtil.hexToTenString(dataList.get(0));

            log.info("[erc20BalanceOf]erc20Address:{}  erc20Balance:{} ", erc20Address, erc20Balance);

            return erc20Balance;

        } catch (Exception e) {
            log.error("[erc20Decimals]erc20Address:{} exception e:{}", erc20Address, e);
        }
        log.info("[erc20Decimals]erc20Address:{} return ZERO ", erc20Address);
        return null;

    }

    public String getDaoCurrentRound(String projectId) {
        // canvas next price
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getContractAddress());
        infuraCallRequestDto.setData(ContractMethodEnum.GET_DAO_CURRENT_ROUND.getMethodAddress() + projectId);

        // 调用查询使用数据集的user
        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[getDaoCurrentRound] error result:{} projectId:{}", result.getResultDesc(), projectId);
            return BigDecimal.ZERO.toPlainString();
        }
        String canvasInfoData = result.getData();
        String price = CommonUtil.hexToTenString(canvasInfoData);
        log.info("[getDaoCurrentRound]projectId:{}, price:{}", projectId, price);
        if (StringUtils.isNotBlank(price)) {
            return price;
        }
        return BigDecimal.ZERO.toPlainString();
    }

    public MintWindowInfoVo getMintWindowInfoVo(Dao dao) {
        MintWindowInfoVo mintWindowInfoVo = new MintWindowInfoVo();

        WorkCountBo workCountBo = workService.selectDrbNftOwnerCountByDaoId(String.valueOf(dao.getId()), Integer.valueOf(dao.getCurrentRound()));
        mintWindowInfoVo.setMintedWorks(workCountBo.getMintedWorks());
        mintWindowInfoVo.setMinters(workCountBo.getMinters());
        if (StringUtils.isNotBlank(workCountBo.getMintFee())) {
            mintWindowInfoVo.setMintFee(workCountBo.getMintFee());
        }

        //实时查询最新的token和eth
        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(), Integer.valueOf(dao.getCurrentRound()));
        if (daoDrbStatistics != null && daoDrbStatistics.getErc20Amount() != null) {
            mintWindowInfoVo.setBlockRewardToken(ProtoDaoCommonUtil.bigdecimalToString(daoDrbStatistics.getErc20Amount()));
            mintWindowInfoVo.setBlockRewardEth(ProtoDaoCommonUtil.bigdecimalToString(daoDrbStatistics.getEthAmount()));
        } else {
            //实时查询最新的token和eth
            BigDecimal remainderErc20 = this.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());
            // BigDecimal remainderEth = this.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals());
            BigDecimal remainderEth = this.getInputToken(dao);
            //无限模式全部发放
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getInfiniteMode())) {
                //不用计算，全部发放
            } else {
                //乐透模式
                if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getRoyaltyTokenLotteryMode()) && !TrueOrFalseEnum.FALSE.getStatus().equals(Integer.valueOf(dao.getCurrentRound()))) {
                    BigDecimal lotteryRount = new BigDecimal(dao.getCurrentRound()).subtract(new BigDecimal(String.valueOf(dao.getLastActiveRound())));
                    BigDecimal mintWind = new BigDecimal(String.valueOf(dao.getRemainingMintWindow())).add(lotteryRount).subtract(BigDecimal.ONE);
                    if (mintWind.compareTo(BigDecimal.ZERO) > 0) {
                        remainderErc20 = remainderErc20.multiply(lotteryRount).divide(mintWind, 18, RoundingMode.HALF_UP);
                        remainderEth = remainderEth.multiply(lotteryRount).divide(mintWind, 18, RoundingMode.HALF_UP);
                    }
                    // remainderErc20 = remainderErc20.multiply(lotteryRount).divide(new BigDecimal(String.valueOf(dao.getRemainingMintWindow())).add(lotteryRount).subtract(BigDecimal.ONE), 18, RoundingMode.HALF_UP);
                    // remainderEth = remainderEth.multiply(lotteryRount).divide(new BigDecimal(String.valueOf(dao.getRemainingMintWindow())).add(lotteryRount).subtract(BigDecimal.ONE), 18, RoundingMode.HALF_UP);
                } else {
                    if (dao.getRemainingMintWindow() != null && dao.getRemainingMintWindow() > 0) {
                        remainderErc20 = remainderErc20.divide(new BigDecimal(String.valueOf(dao.getRemainingMintWindow())), 18, RoundingMode.HALF_UP);
                        remainderEth = remainderEth.divide(new BigDecimal(String.valueOf(dao.getRemainingMintWindow())), 18, RoundingMode.HALF_UP);
                    }
                }
            }
            mintWindowInfoVo.setBlockRewardToken(ProtoDaoCommonUtil.bigdecimalToString(remainderErc20));
            mintWindowInfoVo.setBlockRewardEth(ProtoDaoCommonUtil.bigdecimalToString(remainderEth));
//            mintWindowInfoVo.setBlockRewardToken(ProtoDaoCommonUtil.bigdecimalToString(BigDecimal.ONE));
//            mintWindowInfoVo.setBlockRewardEth(ProtoDaoCommonUtil.bigdecimalToString(BigDecimal.ONE));
        }

        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
            mintWindowInfoVo.setInternalRewardToken(mintWindowInfoVo.getBlockRewardToken());
            mintWindowInfoVo.setInternalRewardEth(mintWindowInfoVo.getBlockRewardEth());
        } else {
            List<DaoAllocationStrategy> daoAllocationStrategies = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
            DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
            daoAllocationStrategy.setRoyaltyProportion(BigDecimal.ONE);
            DaoAllocationStrategy daoAllocationStrategyToken = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.FALSE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())).findFirst().orElseGet(() -> daoAllocationStrategy);
            DaoAllocationStrategy daoAllocationStrategyEth = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.TWO.getType().equals(v.getRoyaltyType())).findFirst().orElseGet(() -> daoAllocationStrategy);

            mintWindowInfoVo.setInternalRewardToken(ProtoDaoCommonUtil.bigdecimalToString(new BigDecimal(mintWindowInfoVo.getBlockRewardToken()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoAllocationStrategyToken.getRoyaltyProportion()))));
            mintWindowInfoVo.setInternalRewardEth(ProtoDaoCommonUtil.bigdecimalToString(new BigDecimal(mintWindowInfoVo.getBlockRewardEth()).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoAllocationStrategyEth.getRoyaltyProportion()))));
        }

        mintWindowInfoVo.setDaoToken(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));


        return mintWindowInfoVo;
    }

    public BasicInformationVo getBasicInformationVo(Dao dao) {

        BasicInformationVo basicInformationVo = new BasicInformationVo();
        Integer nftAmount = workService.selectNftAmounts(dao.getId() + "");
        basicInformationVo.setMintCap(nftAmount);
        basicInformationVo.setTotalMintCap(dao.getTotalNftCasting());
        if (StringUtils.isNotBlank(dao.getCurrentRound())) {
            Integer nftCurrentAmount = workService.selectDrbNftCountByDaoId(dao.getId() + "", Integer.valueOf(dao.getCurrentRound()));
            basicInformationVo.setMintWindowCap(nftCurrentAmount);
        }
        basicInformationVo.setTotalMintWindowCap(dao.getDailyMintCap());
        basicInformationVo.setMintWindowDuration(new BigDecimal(dao.getDuration()).divide(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber), 18, RoundingMode.UP).intValue());
        basicInformationVo.setRemainingMintWindow(dao.getRemainingMintWindow());
        //实时查询最新的token和eth
        BigDecimal remainderErc20 = this.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals());
        BigDecimal remainderEth = this.getInputToken(dao);

//        if (ProtoDaoConstant.DEFAULT_PAY_CURRENCY_TYPE.equals(dao.getPayCurrencyType())){
//            // 如果支付类型为eth，那就查询eth
//            remainderEth = this.ethBalanceOf(dao.getFeePool(),dao.getInputTokenDecimals());
//        }else {
//            // 如果支付类型为其他，查询在input token下的余额
//            remainderEth = this.erc20BalanceOf(dao.getInputTokenAddress(), dao.getFeePool(), dao.getInputTokenDecimals(),null);
//        }

//        BigDecimal remainderErc20 = BigDecimal.ONE;
//        BigDecimal remainderEth = BigDecimal.ONE;
        if (remainderEth != null) {
            basicInformationVo.setSubDaoAssetPoolEth(remainderEth.stripTrailingZeros().toPlainString());
        }
        if (remainderErc20 != null) {
            basicInformationVo.setSubDaoAssetPoolDaoToken(remainderErc20.stripTrailingZeros().toPlainString());
        }

        return basicInformationVo;

    }

    // 1.7 修改，获取dao的InputToken Balance
    public BigDecimal getInputToken(Dao dao) {
        BigDecimal remainderEth = null;

        if (ProtoDaoConstant.DEFAULT_PAY_CURRENCY_TYPE.equals(dao.getPayCurrencyType())) {
            // 如果支付类型为eth，那就查询eth
            remainderEth = this.ethBalanceOf(dao.getFeePool(), dao.getInputTokenDecimals());
        } else {
            // 如果支付类型为其他，查询在input token下的余额
            remainderEth = this.erc20BalanceOf(dao.getInputTokenAddress(), dao.getFeePool(), dao.getInputTokenDecimals(), null);
        }
        return remainderEth;
    }

    public Double blockNoStartTime(String blockNo) {
        //获取当前区块，获取总区块数
        Result<String> result = iSubscriptionService.queryBlockTime(ProtoDaoConstant.netWork, blockNo);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[blockNoStartTime] queryBlockTime error:{}", result.getResultDesc());
            return null;
        }
        String blockTimeStr = result.getData();
//        String blockTime = CommonUtil.hexToTenString(blockTimeStr);
        if (StringUtils.isBlank(blockTimeStr)) {
            log.error("[blockNoStartTime] queryBlockTime error:{}", result.getResultDesc());
            return null;
        }
        return new Double(blockTimeStr);
    }

    /**
     * 查询erc20的holder数量
     *
     * @param erc20TokenAddress
     * @return
     */
    public Integer tokenHoldersBak(String erc20TokenAddress) {
        JsonRpcBo jsonRpcBo = new JsonRpcBo();
        JsonRpcParamBo params = new JsonRpcParamBo();
        params.setBlockchain(multichainChain);
        params.setContractAddress(erc20TokenAddress);
        jsonRpcBo.setParams(params);

        RestTemplate restTemplate = new RestTemplate();
        try {
            log.info("[tokenHolders] jsonRpcBo:{} multichainUrl:{}", JacksonUtil.obj2json(jsonRpcBo), multichainUrl);
            HttpHeaders headers = new HttpHeaders();
            MediaType type = MediaType.parseMediaType("application/json; charset=UTF-8");
            headers.setContentType(type);
            headers.add("Connection", "keep-alive");
            headers.add("Accept", "*/*");
            headers.add("User-Agent", "PostmanRuntime/7.36.1");
            HttpEntity<String> request = new HttpEntity<String>(JacksonUtil.obj2json(jsonRpcBo), headers);

            String responseEntity = restTemplate.postForObject(multichainUrl, request, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(responseEntity, JSONObject.class);
            if (jsonObject == null) {
                log.info("[tokenHolders] responseEntity:{}", responseEntity);
                log.error("[tokenHolders] jsonObject is null, erc20TokenAddress:{}", erc20TokenAddress);
                return 0;
            }

            JSONObject result = jsonObject.getJSONObject("result");
            if (result == null || result.size() == 0) {
                log.error("[tokenHolders] result is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return 0;
            }
            String holdersCount = result.getString("holdersCount");
            if (StringUtils.isBlank(holdersCount)) {
                log.error("[tokenHolders] result is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return 0;
            }
            log.info("[tokenHolders] erc20TokenAddress:{} holdersCount:{}", erc20TokenAddress, holdersCount);
            return Integer.valueOf(holdersCount);

        } catch (Exception e) {
            log.error("[blockTime] exception erc20TokenAddress:{} e:", erc20TokenAddress, e);
        }

        return 0;
    }

    public Integer tokenHolders(String erc20TokenAddress) {
        String url = String.format(multichainUrl, erc20TokenAddress);

        RestTemplate restTemplate = new RestTemplate();
        try {
            log.info("[tokenHolders] url:{}", url);
            String username = multichainKey;
            String password = "";

            HttpHeaders headers = new HttpHeaders();
            MediaType type = MediaType.parseMediaType("application/json; charset=UTF-8");
            headers.setContentType(type);
            headers.add("Connection", "keep-alive");
            headers.add("Accept", "*/*");
            headers.add("User-Agent", "PostmanRuntime/7.36.1");
            headers.setBasicAuth(username, password);
            HttpEntity<String> request = new HttpEntity<String>(headers);

            ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.GET, request, String.class);
            String responseEntity = response.getBody();
            JSONObject jsonObject = JacksonUtil.json2pojo(responseEntity, JSONObject.class);
            if (jsonObject == null) {
                log.info("[tokenHolders] responseEntity:{}", responseEntity);
                log.error("[tokenHolders] jsonObject is null, erc20TokenAddress:{}", erc20TokenAddress);
                return 0;
            }

            JSONObject data = jsonObject.getJSONObject("data");
            if (data == null || data.size() == 0) {
                log.error("[tokenHolders] result is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return 0;
            }
            String holdersCount = data.getJSONObject("pagination").getString("total_count");
            if (StringUtils.isBlank(holdersCount)) {
                log.error("[tokenHolders] result is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return 0;
            }
            log.info("[tokenHolders] erc20TokenAddress:{} holdersCount:{}", erc20TokenAddress, holdersCount);
            return Integer.valueOf(holdersCount);

        } catch (Exception e) {
            log.error("[blockTime] exception erc20TokenAddress:{} e:", erc20TokenAddress, e);
        }

        return 0;
    }

    /**
     * 查询erc20的某个时间段的收入
     *
     * @param erc20TokenAddress
     * @return
     */
    public BigDecimal searchTokenIncome(String startTime, String endTime, String feePool, String erc20TokenAddress) {
        // TODO 外部接口不支持sepolia，所以默认返回0，如上主网，需要提前测试
        if (!EnvEnum.PROD.getEnv().equals(ProtoDaoConstant.activity)) {
            log.info("[searchTokenIncome] return 0 in env {}", ProtoDaoConstant.activity);
            return BigDecimal.ZERO;
        }

        RestTemplate restTemplate = new RestTemplate();
        try {
            log.info("[searchTokenIncome] startTime:{} endTime:{} feePool:{} erc20TokenAddress:{}", startTime, endTime, feePool, erc20TokenAddress);
            if (StringUtils.isAnyBlank(startTime, endTime, feePool, erc20TokenAddress)) {
                return BigDecimal.ZERO;
            }
            HttpHeaders headers = new HttpHeaders();
            MediaType type = MediaType.parseMediaType("application/json; charset=UTF-8");
            headers.setContentType(type);
            headers.add("Connection", "keep-alive");
            headers.add("Accept", "*/*");
            headers.add("User-Agent", "PostmanRuntime/7.36.1");
            headers.add("Content-Type", "application/json");
            headers.add("x-api-key", bitqueryApiKey);
//            headers.add("x-api-key", "BQYWxFenMejj63v9dAJEQs39BdoAmhZ6");
//            String requestParam = String.format("{\"query\":\"query MyQuery($network:EthereumNetwork,$tokenAddress:String,$receiver:String,$startTime:ISO8601DateTime,$endTime:ISO8601DateTime) {\\n  ethereum(network: $network) {\\n    transfers(\\n      receiver: {is: $receiver}\\n      currency: {is: $tokenAddress}\\n      time: {since: $startTime, till: $endTime}\\n      amount: {gt: 0}\\n    ) {\\n      amount(calculate: sum)\\n    }\\n  }\\n}\\n\",\"variables\":{\"network\":\"%s\",\"startTime\":\"%sT00:00:00Z\",\"endTime\":\"%sT00:00:00Z\",\"receiver\":\"%s\",\"tokenAddress\":\"%s\"}}", ProtoDaoConstant.netWork, startTime, endTime, feePool, erc20TokenAddress);
            String requestParam = String.format(queryTokenReceiver, ProtoDaoConstant.netWork, startTime, endTime, feePool, erc20TokenAddress);
            requestParam = requestParam.replaceAll("\\\\", "");
            requestParam = requestParam.replaceAll("\n", "");
            log.info("[searchTokenIncome] requestParam:{}", requestParam);
            HttpEntity<String> request = new HttpEntity<>(requestParam, headers);
            log.info("[searchTokenIncome] bitqueryApiKey:{} bitqueryUrl:{}", bitqueryApiKey, bitqueryUrl);
            String responseEntity = restTemplate.postForObject(bitqueryUrl, request, String.class);
//            String responseEntity = restTemplate.postForObject("https://graphql.bitquery.io/", request, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(responseEntity, JSONObject.class);
            if (jsonObject == null) {
                log.info("[searchTokenIncome] responseEntity:{}", responseEntity);
                log.error("[searchTokenIncome] jsonObject is null, erc20TokenAddress:{}", erc20TokenAddress);
                return BigDecimal.ZERO;
            }

            JSONObject data = jsonObject.getJSONObject("data");
            if (data == null) {
                log.error("[searchTokenIncome] data is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }
            JSONObject ethereum = data.getJSONObject("ethereum");
            if (ethereum == null) {
                log.error("[searchTokenIncome] ethereum is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }

            Object transfers = ethereum.get("transfers");
            if (transfers == null || "null".equals(transfers.toString())) {
                log.info("[searchTokenIncome] transfers is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }

            JSONArray jsonArray = ethereum.getJSONArray("transfers");
            Map<String, Object> holdersCount = JacksonUtil.json2map(jsonArray.get(0).toString());
            if (holdersCount.keySet().size() == 0) {
                log.error("[searchTokenIncome] result is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }
            log.info("[searchTokenIncome] feePool:{} erc20TokenAddress:{} amount:{}", feePool, erc20TokenAddress, holdersCount.get("amount"));
            return new BigDecimal(String.valueOf(holdersCount.get("amount")));

        } catch (Exception e) {
            log.error("[searchTokenIncome] exception erc20TokenAddress:{} e:", erc20TokenAddress, e);
        }

        return BigDecimal.ZERO;
    }

    /**
     * 查询erc20的某个时间段的支出
     *
     * @param erc20TokenAddress
     * @return
     */
    public BigDecimal searchTokeCost(String startTime, String endTime, String feePool, String erc20TokenAddress) {
        // TODO 外部接口不支持sepolia，所以默认返回0，如上主网，需要提前测试
        if (!EnvEnum.PROD.getEnv().equals(ProtoDaoConstant.activity)) {
            log.info("[searchTokeCost] return 0 in env {}", ProtoDaoConstant.activity);
            return BigDecimal.ZERO;
        }

        RestTemplate restTemplate = new RestTemplate();
        try {
            log.info("[searchTokeCost] startTime:{} endTime:{} feePool:{} erc20TokenAddress:{}", startTime, endTime, feePool, erc20TokenAddress);
            if (StringUtils.isAnyBlank(startTime, endTime, feePool, erc20TokenAddress)) {
                return BigDecimal.ZERO;
            }
            HttpHeaders headers = new HttpHeaders();
            MediaType type = MediaType.parseMediaType("application/json; charset=UTF-8");
            headers.setContentType(type);
            headers.add("Connection", "keep-alive");
            headers.add("Accept", "*/*");
            headers.add("User-Agent", "PostmanRuntime/7.36.1");
            headers.add("Content-Type", "application/json");
//            headers.add("x-api-key", "BQYWxFenMejj63v9dAJEQs39BdoAmhZ6");//单元测试用
            headers.add("X-API-KEY", bitqueryApiKey);
//            String requestParam = String.format("{\"query\":\"query MyQuery($network:EthereumNetwork,$tokenAddress:String,$receiver:String,$startTime:ISO8601DateTime,$endTime:ISO8601DateTime) {\\n  ethereum(network: $network) {\\n    transfers(\\n      sender: {is: $receiver}\\n      currency: {is: $tokenAddress}\\n      time: {since: $startTime, till: $endTime}\\n      amount: {gt: 0}\\n    ) {\\n      amount(calculate: sum)\\n    }\\n  }\\n}\\n\",\"variables\":{\"network\":\"%s\",\"startTime\":\"%sT00:00:00Z\",\"endTime\":\"2%sT00:00:00Z\",\"receiver\":\"%s\",\"tokenAddress\":\"%s\"}}", ProtoDaoConstant.netWork, startTime, endTime, feePool, erc20TokenAddress);
            String requestParam = String.format(queryTokenSender, ProtoDaoConstant.netWork, startTime, endTime, feePool, erc20TokenAddress);
            requestParam = requestParam.replaceAll("\\\\", "");
            requestParam = requestParam.replaceAll("\n", "");
            log.info("[searchTokeCost] requestParam:{}", requestParam);
            HttpEntity<String> request = new HttpEntity<>(requestParam, headers);

            String responseEntity = restTemplate.postForObject("https://graphql.bitquery.io/", request, String.class);
//            String responseEntity = restTemplate.postForObject(bitqueryUrl, request, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(responseEntity, JSONObject.class);
            if (jsonObject == null) {
                log.info("[searchTokeCost] responseEntity:{}", responseEntity);
                log.error("[searchTokeCost] jsonObject is null, erc20TokenAddress:{}", erc20TokenAddress);
                return BigDecimal.ZERO;
            }

            JSONObject data = jsonObject.getJSONObject("data");
            if (data == null) {
                log.error("[searchTokeCost] data is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }
            JSONObject ethereum = data.getJSONObject("ethereum");
            if (ethereum == null) {
                log.error("[searchTokeCost] ethereum is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }
            Object transfers = ethereum.get("transfers");
            if (transfers == null || "null".equals(transfers.toString())) {
                log.info("[searchTokeCost] transfers is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }

            JSONArray jsonArray = ethereum.getJSONArray("transfers");

            Map<String, Object> holdersCount = JacksonUtil.json2map(jsonArray.get(0).toString());
            if (holdersCount.keySet().size() == 0) {
                log.error("[searchTokeCost] result is null, erc20TokenAddress:{} jsonObject:{}", erc20TokenAddress, jsonObject);
                return BigDecimal.ZERO;
            }
            log.info("[searchTokeCost] feePool:{} erc20TokenAddress:{} amount:{}", feePool, erc20TokenAddress, holdersCount.get("amount"));
            return new BigDecimal(String.valueOf(holdersCount.get("amount")));

        } catch (Exception e) {
            log.error("[searchTokeCost] exception erc20TokenAddress:{} e:", erc20TokenAddress, e);
        }

        return BigDecimal.ZERO;
    }

    /**
     * 查询eth的某个时间段的收入
     *
     * @param feePool
     * @return
     */
    public BigDecimal searchEthIncome(String startTime, String endTime, String feePool) {
        // TODO 外部接口不支持sepolia，所以默认返回0，如上主网，需要提前测试
        if (!EnvEnum.PROD.getEnv().equals(ProtoDaoConstant.activity)) {
            log.info("[searchEthIncome] return 0 in env {}", ProtoDaoConstant.activity);
            return BigDecimal.ZERO;
        }

        RestTemplate restTemplate = new RestTemplate();
        try {
            log.info("[searchEthIncome] startTime:{} endTime:{} feePool:{} ", startTime, endTime, feePool);
            if (StringUtils.isAnyBlank(startTime, endTime, feePool)) {
                return BigDecimal.ZERO;
            }
            HttpHeaders headers = new HttpHeaders();
            MediaType type = MediaType.parseMediaType("application/json; charset=UTF-8");
            headers.setContentType(type);
            headers.add("Connection", "keep-alive");
            headers.add("Accept", "*/*");
            headers.add("User-Agent", "PostmanRuntime/7.36.1");
            headers.add("Content-Type", "application/json");
            headers.add("X-API-KEY", bitqueryApiKey);
//            headers.add("X-API-KEY", "BQYWxFenMejj63v9dAJEQs39BdoAmhZ6");
//            String requestParam = String.format("{\"query\":\"query ($network: EthereumNetwork!, $from: ISO8601DateTime, $to: ISO8601DateTime, $receiver: String,$token: [String!] ) {\\n  ethereum(network: $network) {\\n    transfers(\\n      receiver: {is: $receiver}\\n      currency: {in: $token}\\n      time: {since: $from, till: $to}\\n      options: {asc: \\\"currency.symbol\\\"}\\n    ) {\\n      amount(calculate: sum)\\n      currency {\\n        symbol\\n      }\\n    }\\n  }\\n}\\n\",\"variables\":\"{\\n  \\\"network\\\": \\\"%s\\\",\\n  \\\"from\\\": \\\"%sT00:00:00Z\\\",\\n  \\\"to\\\": \\\"%sT00:00:00Z\\\",\\n  \\\"receiver\\\":\\\"%s\\\",\\n  \\\"token\\\":\\\"%s\\\"\\n}\"}", "goerli", startTime, endTime, feePool, "GTH");
            String requestParam = String.format(queryEthReceiver, ProtoDaoConstant.netWork, startTime, endTime, feePool, queryEthToken);
            requestParam = requestParam.replaceAll("qwert", "\\\\");
            requestParam = requestParam.replaceAll("\n", "");
            log.info("[searchEthIncome] requestParam:{}", requestParam);
            HttpEntity<String> request = new HttpEntity<>(requestParam, headers);

//            String responseEntity = restTemplate.postForObject("https://graphql.bitquery.io/", request, String.class);
            String responseEntity = restTemplate.postForObject(bitqueryUrl, request, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(responseEntity, JSONObject.class);
            if (jsonObject == null) {
                log.info("[searchEthIncome] responseEntity:{}", responseEntity);
                log.error("[searchEthIncome] jsonObject is null, feePool:{}", feePool);
                return BigDecimal.ZERO;
            }

            JSONObject data = jsonObject.getJSONObject("data");
            if (data == null) {
                log.error("[searchEthIncome] data is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                return BigDecimal.ZERO;
            }
            JSONObject ethereum = data.getJSONObject("ethereum");
            if (ethereum == null) {
                log.error("[searchEthIncome] ethereum is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                return BigDecimal.ZERO;
            }

            Object transfers = ethereum.get("transfers");
            if (transfers == null || "null".equals(transfers.toString())) {
                log.info("[searchEthIncome] transfers is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                return BigDecimal.ZERO;
            }

            JSONArray jsonArray = ethereum.getJSONArray("transfers");
            BigDecimal amount = BigDecimal.ZERO;
            for (Object obj : jsonArray) {
                Map<String, Object> holdersCount = JacksonUtil.json2map(obj.toString());
                if (holdersCount == null || holdersCount.keySet().size() == 0) {
                    log.error("[searchEthIncome] result is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                    return BigDecimal.ZERO;
                }
                amount = amount.add(new BigDecimal(String.valueOf(holdersCount.get("amount"))));
            }

            log.info("[searchEthIncome] feePool:{} amount:{}", feePool, amount);
            return amount;

        } catch (Exception e) {
            log.error("[searchEthIncome] exception feePool:{} e:", feePool, e);
        }

        return BigDecimal.ZERO;
    }

    /**
     * 查询eth的某个时间段的支出
     *
     * @param feePool
     * @return
     */
    public BigDecimal searchEthCost(String startTime, String endTime, String feePool) {
        // TODO 外部接口不支持sepolia，所以默认返回0，如上主网，需要提前测试
        if (!EnvEnum.PROD.getEnv().equals(ProtoDaoConstant.activity)) {
            log.info("[searchEthCost] return 0 in env {}", ProtoDaoConstant.activity);
            return BigDecimal.ZERO;
        }

        RestTemplate restTemplate = new RestTemplate();
        try {
            log.info("[searchEthCost] startTime:{} endTime:{} feePool:{} ", startTime, endTime, feePool);
            if (StringUtils.isAnyBlank(startTime, endTime, feePool)) {
                return BigDecimal.ZERO;
            }
            HttpHeaders headers = new HttpHeaders();
            MediaType type = MediaType.parseMediaType("application/json; charset=UTF-8");
            headers.setContentType(type);
            headers.add("Connection", "keep-alive");
            headers.add("Accept", "*/*");
            headers.add("User-Agent", "PostmanRuntime/7.36.1");
            headers.add("Content-Type", "application/json");
            headers.add("X-API-KEY", bitqueryApiKey);
//            headers.add("X-API-KEY", "BQYWxFenMejj63v9dAJEQs39BdoAmhZ6");
//            String requestParam = String.format("{\"query\":\"query ($network: EthereumNetwork!, $from: ISO8601DateTime, $to: ISO8601DateTime, $receiver: String,$token: [String!] ) {\\n  ethereum(network: $network) {\\n    transfers(\\n      sender: {is: $receiver}\\n      currency: {in: $token}\\n      time: {since: $from, till: $to}\\n      options: {asc: \\\"currency.symbol\\\"}\\n    ) {\\n      amount(calculate: sum)\\n      currency {\\n        symbol\\n      }\\n    }\\n  }\\n}\\n\",\"variables\":\"{\\n  \\\"network\\\": \\\"%s\\\",\\n  \\\"from\\\": \\\"%sT00:00:00Z\\\",\\n  \\\"to\\\": \\\"%sT00:00:00Z\\\",\\n  \\\"receiver\\\":\\\"%s\\\",\\n  \\\"token\\\":\\\"%s\\\"\\n}\"}", ProtoDaoConstant.netWork, startTime, endTime, feePool, "GTH");
            String requestParam = String.format(queryEthSender, ProtoDaoConstant.netWork, startTime, endTime, feePool, queryEthToken);
            requestParam = requestParam.replaceAll("qwert", "\\\\");
            requestParam = requestParam.replaceAll("\n", "");
            log.info("[searchEthCost] bitqueryApiKey:{} requestParam:{}", bitqueryApiKey, requestParam);
            HttpEntity<String> request = new HttpEntity<>(requestParam, headers);

            String responseEntity = restTemplate.postForObject(bitqueryUrl, request, String.class);
//            String responseEntity = restTemplate.postForObject("https://graphql.bitquery.io/", request, String.class);
            JSONObject jsonObject = JacksonUtil.json2pojo(responseEntity, JSONObject.class);
            if (jsonObject == null) {
                log.info("[searchEthCost] responseEntity:{}", responseEntity);
                log.error("[searchEthCost] jsonObject is null, feePool:{}", feePool);
                return BigDecimal.ZERO;
            }

            JSONObject data = jsonObject.getJSONObject("data");
            if (data == null) {
                log.error("[searchEthCost] data is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                return BigDecimal.ZERO;
            }
            JSONObject ethereum = data.getJSONObject("ethereum");
            if (ethereum == null) {
                log.error("[searchEthCost] ethereum is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                return BigDecimal.ZERO;
            }

            Object transfers = ethereum.get("transfers");
            if (transfers == null || "null".equals(transfers.toString())) {
                log.info("[searchEthCost] transfers is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                return BigDecimal.ZERO;
            }
            JSONArray jsonArray = ethereum.getJSONArray("transfers");
            BigDecimal amount = BigDecimal.ZERO;
            for (Object obj : jsonArray) {
                Map<String, Object> holdersCount = JacksonUtil.json2map(obj.toString());
                if (holdersCount == null || holdersCount.keySet().size() == 0) {
                    log.error("[searchEthIncome] result is null, feePool:{} jsonObject:{}", feePool, jsonObject);
                    return BigDecimal.ZERO;
                }
                amount = amount.add(new BigDecimal(String.valueOf(holdersCount.get("amount"))));
            }
            log.info("[searchEthCost] feePool:{} amount:{}", feePool, amount);
            return amount;

        } catch (Exception e) {
            log.error("[searchEthCost] exception feePool:{} e:", feePool, e);
        }

        return BigDecimal.ZERO;
    }

    public BigDecimal getErc20TotalSupply(String erc20Address, Integer erc20TokenDecimals) {

        if (StringUtils.isBlank(erc20Address)) {
            return BigDecimal.ZERO;
        }
        // canvas next price
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(erc20Address);
        infuraCallRequestDto.setData(ContractMethodEnum.PROJECT_TOTAL_SUPPLY.getMethodAddress());

        // 调用查询使用数据集的user
        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[getErc20TotalSupply] error result:{} erc20Address:{}", result.getResultDesc(), erc20Address);
            return BigDecimal.ZERO;
        }
        String canvasInfoData = result.getData();
        String totalSupply = CommonUtil.hexToTenString(canvasInfoData);
        log.info("[getErc20TotalSupply]erc20Address:{}, totalSupply:{}", erc20Address, totalSupply);
        if (StringUtils.isNotBlank(totalSupply)) {
            if (erc20TokenDecimals == null) {
                erc20TokenDecimals = 18;
            }
            return new BigDecimal(totalSupply).divide(new BigDecimal("10").pow(erc20TokenDecimals));
        }
        return BigDecimal.ZERO;
    }

    public BigDecimal getDaoLastModifyRound(Dao dao) {

        try {
            // 现在也能够给出重启的round，function getDaoLastModifyRound(bytes32 daoId) public view returns (uint256)
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.getDaoLastModifyRound.getContractAddress());
            infuraCallRequestDto.setData(ContractMethodEnum.getDaoLastModifyRound.getMethodAddress() + dao.getProjectId());


            Result<String> getRewardTillRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (getRewardTillRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[getDaoLastModifyRound]infura getDaoLastActiveRound return data:{}", getRewardTillRoundResult.getData());
                String claim = getRewardTillRoundResult.getData();
                String claimPrice = CommonUtil.hexToTenString(claim);
                return new BigDecimal(claimPrice);
            } else {
                log.error("[getDaoLastModifyRound]infura getDaoLastActiveRound return data:{} desc:{} daoId:{}",
                        getRewardTillRoundResult.getData(), getRewardTillRoundResult.getResultDesc(), dao.getId());
            }

        } catch (Exception e) {
            log.error("[getDaoLastModifyRound]infura getDaoLastActiveRound error daoId:{} e:{} ", dao.getId(), e);
            //如果失败了先置为0，之后修改数据库
        }
        return BigDecimal.ZERO;
    }

    public Boolean getTopUpNftLockedStatus(Dao dao, Work work) {
        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ContractMethodEnum.checkTopUpNftLockedStatus.getContractAddress());
            // erc721Address,tokenId
            infuraCallRequestDto.setData(ContractMethodEnum.getDaoLastModifyRound.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32(dao.getErc721Token()) + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(work.getWorkNumber())));

            Result<String> workLockStatusResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
            if (workLockStatusResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
                log.info("[getDaoLastModifyRound]infura getTopUpNftLockedStatus return data:{}", workLockStatusResult.getData());
                String lockStatus = CommonUtil.hexToTenString(workLockStatusResult.getData());
                return WorkLockStatusEnum.LOCK.getStatus().equals(new Integer(lockStatus));
            } else {
                log.error("[getDaoLastModifyRound]infura getTopUpNftLockedStatus return data:{} desc:{} daoId:{}",
                        workLockStatusResult.getData(), workLockStatusResult.getResultDesc(), dao.getId());
            }

        } catch (Exception e) {
            log.error("[getTopUpNftLockedStatus]infura getTopUpNftLockedStatus error daoId:{} e:{} ", dao.getId(), e.getMessage());
        }
        return false;
    }

    public Integer getErc20DecimalIfErc20PayModel(Dao dao) {
        if (dao.getErc20PaymentMode() == 1) {
            if (dao.getErc20TokenDecimals() != null) {
                return dao.getErc20TokenDecimals();
            } else {
                return 18;
            }
        }
        return null;
    }

    // 获取plan的当前周期
    public Integer getPlanCurrentRound(String planCode) {
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getContractAddress());
        infuraCallRequestDto.setData(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getMethodAddress() + planCode);
        infuraCallRequestDto.setFrom(ProtoDaoConstant.ZERO_ADDRESS);
        log.info("[getPlanCurrentRound] call data:{}", JacksonUtil.obj2json(infuraCallRequestDto));
        Result<String> planRoundResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (planRoundResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
            log.info("[getPlanCurrentRound]infura getPlanCurrentRound return data:{}", planRoundResult.getData());
            String round = CommonUtil.hexToTenString(planRoundResult.getData());

            if (round != null) {
                return Integer.valueOf(round);
            }
            log.error("[getPlanCurrentRound] hex to ten is error data:{}", planRoundResult.getData());
        } else {
            log.error("[getPlanCurrentRound]infura getPlanCurrentRound return data:{} desc:{} planCode:{}",
                    planRoundResult.getData(), planRoundResult.getResultDesc(), planCode);
        }
        return 0;
    }

    public BigDecimal getPlanNftReward(String planCode, String erc721Token, Integer workNumber, Integer decimal) throws Exception {
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.CLAIM_MULTI_PLAN_REWARD.getContractAddress());
        // bytes32[] calldata planIds
        // NftIdentifier calldata nft
        // offset + 721 address + tokenId + size + planCode
        infuraCallRequestDto.setData(ContractMethodEnum.CLAIM_MULTI_PLAN_REWARD.getMethodAddress() + CommonUtil.fillLeadingZerosInBytes32("60") + CommonUtil.fillLeadingZerosInBytes32(erc721Token) + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.tenToHex(workNumber)) + CommonUtil.fillLeadingZerosInBytes32("1") + CommonUtil.removeHexPrefixIfExists(planCode));
        log.info("[getPlanNftReward]infura claimMultiPlanReward call data:{}", JacksonUtil.obj2json(infuraCallRequestDto));
        Result<String> rewardResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (rewardResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
            log.info("[getPlanNftReward]infura claimMultiPlanReward return data:{}", rewardResult.getData());

            String amount = CommonUtil.hexToTenString(rewardResult.getData());

            return new BigDecimal(amount).divide(CommonUtil.getPowBigDecimal(decimal), 18, RoundingMode.HALF_UP);
        } else {
            log.error("[getPlanNftReward]infura claimMultiPlanReward return data:{} desc:{} planCode:{},erc721Token:{},workNumber:{}",
                    rewardResult.getData(), rewardResult.getResultDesc(), planCode, erc721Token, workNumber);
        }
        return BigDecimal.ZERO;
    }
}
