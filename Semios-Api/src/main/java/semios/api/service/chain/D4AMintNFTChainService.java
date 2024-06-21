package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.response.MintWorkUriDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.*;
import semios.api.model.enums.*;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.DateUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @description: 铸造NFT
 * @author: xiangbin
 * @create: 2022-08-25 09:37
 **/
@Slf4j
@Service
public class D4AMintNFTChainService implements SubscriberChainService {

    private static final RestTemplate restTemplate = new RestTemplate();
    @Autowired
    private IWorkService workService;
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private IDaoService daoService;
    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;
    @Autowired
    private ICanvasDrbStatisticsService canvasDrbStatisticsService;
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    @Autowired
    private S3Service s3Service;

    @Autowired
    @Qualifier("restTemplateSSl")
    private RestTemplate restTemplateSSl;

    @Value("${work_image_url}")
    private String workImageUrl;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    @Autowired
    private ITokenReceivedRecordService tokenReceivedRecordService;


    private ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

    public static void main(String[] args) {

        // Double aa = 0.0;
        // System.out.println(new BigDecimal(aa).add(new BigDecimal("0.005")).toPlainString());
        // System.out.println(new BigDecimal("0.02")
        // .multiply(BigDecimal.ONE.subtract(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
        // .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
        // .add(new BigDecimal(ProtoDaoConstant.MINT_D4A_FEE_RATIO)
        // .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))))
        // .toPlainString());
        // String workUrl = "https://test-protodao.s3.ap-southeast-1.amazonaws.com/AGypxRfW9s3p9kh5DSz3g16522.json";
        // System.out.println(workUrl.substring(workUrl.lastIndexOf("/")+1));

        String data =
                "c293cfd4fcc160f52fe2fa18878b6a3b257be25492e458847b9f095ea687a64ac7ee4e1f6e72fdeb050714c04f5db8629e250485d12b1414c6c748bce7a48997000000000000000000000000000000000000000000000000000000000000000b00000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000002386f26fc10000000000000000000000000000000000000000000000000000000000000000005768747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f776f726b2f5731373031333135333537333635353238302e6a736f6e000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String token_id = CommonUtil.hexToTenString(dataList.get(2));
        String workUri = CommonUtil.dynamicArgumentDecoding(data, dataList.get(3), true);
        String price = CommonUtil.hexToTenString(dataList.get(4));

    }

    /*
     * dao:dao信息
     * work:mint work
     * transactionDto:交易信息
     * toAdress:受益人 canvas create owner
     * */

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[D4AMintNFTChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String token_id = CommonUtil.hexToTenString(dataList.get(2));
        String workUri = CommonUtil.dynamicArgumentDecoding(data, dataList.get(3), true);
        String price = CommonUtil.hexToTenString(dataList.get(4));
        String mount_dao_721tokenAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(5))); //address erc721Address; 绑定的dao 721tokenAddress
        String mount_work_number = CommonUtil.hexToTenString(dataList.get(6));//uint256 tokenId;  绑定的workNumber

        Work work = workService.selectWorkByUri(workUri);
        if (work == null) {
            log.error("[D4AMintNFTChainService]根据workUri未查询到work记录workUri:{}", workUri);
            throw new RuntimeException("未查询到对应的work信息");
        }
        if (WorkStatusEnum.CASTED.getStatus().equals(work.getWorkStatus())) {
            log.error("[D4AMintNFTChainService]根据workUri查询到work记录已铸造workUri:{}", workUri);
            return;
        }
        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[D4AMintNFTChainService]未查询到dao记录projectId:{}", projectId);
            throw new RuntimeException("未查询到对应的dao信息");
        }
        Canvas canvas = canvasService.selectCanvasDetailByCanvasId(canvasId);
        if (canvas == null) {
            log.error("[D4AMintNFTChainService]未查询到canvas记录canvasId:{}", canvasId);
            throw new RuntimeException("未查询到对应的canvas信息");
        }

        // 替换为从合约冲查询project的周期
        String currentRound = commonService.getDaoCurrentRound(projectId);
        log.info("从合约中获取的projectId:{},currentRound:{}", projectId, currentRound);
        Integer currentDrb = Integer.parseInt(dao.getCurrentRound());
        if (Integer.parseInt(currentRound) != currentDrb) {
            log.info("从合约中获取的projectId:{},currentRound:{},数据库中的drb:{}", projectId, currentRound, currentDrb);
            currentDrb = Integer.parseInt(currentRound);
        }

        // 回写 mintedAddress
        MintWorkUriDto mintWorkUriDto = restTemplate.getForObject(workUri, MintWorkUriDto.class);

        // mint address
        if (StringUtils.isNotBlank(dao.getErc721Token())) {
//            InfuraCallRequestDto indexRequest = new InfuraCallRequestDto();
//            indexRequest.setNetWork(ProtoDaoConstant.netWork);
//            indexRequest.setTo(dao.getErc721Token());
//            indexRequest.setData(ContractMethodEnum.MINT_ADDRESS.getMethodAddress() + dataList.get(2));
//
//            // 调用查询使用数据集的user
//            Result<String> indexResult = iSubscriptionService.infuraCall(indexRequest);
            Result<String> result = iSubscriptionService.ethGetTransactionByHash(ProtoDaoConstant.netWork,
                    CommonUtil.addHexPrefixIfNotExist(transactionDto.getTransactionHash()));
            log.info("[D4AMintNFTChainService] result:{}", result.getData());
            if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[D4AMintNFTChainService] error result:{}", result.getResultDesc());
                throw new RuntimeException("GetTransactionByHash error");
            }
            Map<String, Object> objectMap = JacksonUtil.json2map(result.getData());
            if (objectMap == null) {
                log.error("[D4AMintNFTChainService] objectMap is null result:{}", result.getData());
                throw new RuntimeException("GetTransactionByHash objectMap is null");
            }
            String fromAddress = (String) objectMap.get("from");
//            fromAddress = CommonUtil.addHexPrefixIfNotExist(fromAddress.toLowerCase());

            log.info("[D4AMintNFTChainService]infura fromAddress:{}", fromAddress);
            String mintAddress = CommonUtil.addHexPrefixIfNotExist(fromAddress.toLowerCase());
            work.setMintedAddress(mintAddress);
            work.setOwnerAddress(mintAddress);
//            mintWorkUriDto.setName("D4A@" + dao.getDaoNumber() + "/Canvas*" + canvas.getCanvasNumber() + "/NFT#"
//                    + Integer.valueOf(token_id));
            mintWorkUriDto.setName(dao.getDaoName() + "." + Integer.valueOf(token_id));
            mintWorkUriDto.setMintedAddress(mintAddress);
            String fileName = workUri.substring(workUri.lastIndexOf("/") + 1);
            BucketObjectRepresentaion representaion = new BucketObjectRepresentaion();
            representaion.setObjectName(fileName);
            representaion.setText(JacksonUtil.obj2json(mintWorkUriDto));
            s3Service.putObject(
                    ProtoDaoConstant.bucketName + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName,
                    representaion);

            // 30秒后执行
            executorService.schedule(() -> {
                // 刷新oponsea名称
                String openseaUrl =
                        String.format(ProtoDaoConstant.openseaApiNftLink, dao.getErc721Token(), token_id);
                log.info("[D4AMintNFTChainService]openseaUrl:{}", openseaUrl);
                HttpHeaders headers = new HttpHeaders();
                headers.set("user-agent", "Chrome/83.0.4103.116");
                if (StringUtils.isNotBlank(ProtoDaoConstant.openseaApiKey)) {
                    headers.set("X-API-KEY", ProtoDaoConstant.openseaApiKey);
                }
                HttpEntity<String> httpEntity = new HttpEntity<>(headers);

                try {
                    ResponseEntity<String> responseEntity =
                            restTemplate.exchange(openseaUrl, HttpMethod.POST, httpEntity, String.class);

                    String response = responseEntity.getBody();
                    if (responseEntity.getStatusCode() != HttpStatus.OK || response == null || !response.contains(ProtoDaoConstant.openseaApiSuccess)) {
                        throw new RuntimeException("保存work刷新workuri失败");
                    }
                    // Map<String, Object> stringObjectMap = JacksonUtil.json2map(responseEntity.getBody());
                    // String name = (String) stringObjectMap.get("name");
                    log.info("[D4AMintNFTChainService]response:{}", response);

                } catch (Exception e) {
                    log.error("[D4AMintNFTChainService] fresh openseaUrl:{} error:{}", openseaUrl, e);
                }
            }, 30, TimeUnit.SECONDS);

        }

        work.setWorkNumber(Integer.valueOf(token_id));
        work.setImageUrl(mintWorkUriDto.getImage());
        work.setWorkDescription(mintWorkUriDto.getDescription());
        work.setCanvasId(CommonUtil.removeHexPrefixIfExists(canvasId));
        work.setCanvasNumber(canvas.getCanvasNumber());
        work.setDaoId(dao.getId());
        work.setProjectId(projectId);
        work.setDaoNumber(dao.getDaoNumber());
        work.setTransactionHash(transactionDto.getTransactionHash());

        log.info("log拿到的price:{}", price);
        BigDecimal mintedPrice;
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
            mintedPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()));
        } else {
            mintedPrice = new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()));
        }
        log.info("计算出来的mintedPrice:{}", mintedPrice);

        work.setMintedPrice(mintedPrice);
        work.setCanId(canvas.getId());
        work.setBlockTime(transactionDto.getBlockTime());
        work.setBlockNumber(transactionDto.getBlockNumber());
        work.setDrbNumber(currentDrb);

        work.setWorkUri(workUri);
        work.setWorkStatus(WorkStatusEnum.CASTED.getStatus());
        // work.setCreateTime(LocalDateTime.now());
        work.setIsDel(0);
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
            work.setTopupMode(WorkTopupModeEnum.MINT_TOPUP_YES.getStatus());
        } else {
            //1.3 topup mode下，可能maindao不是topup，但是subdao是topup  只有MINT_TOPUP_NO模式的work需要更新余额
            Boolean existDao = daoService.selectTopupDaoByExistDaoId(StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId());
            if (existDao != null && existDao) {
                work.setTopupMode(WorkTopupModeEnum.MINT_TOPUP_NO.getStatus());
            }
        }

        boolean update = workService.updateById(work);
        if (!update) {
            log.error("[D4AMintNFTChainService]work状态已变更，不做处理 workUri:{}", workUri);
            return;
        }

        // 添加两个drb static记录
        DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(), currentDrb);
        if (daoDrbStatistics == null) {
            log.info("获取到到daoDrbStatistics为空，重新定义");
            daoDrbStatistics = new DaoDrbStatistics();
            //  如果当前周期的drb记录不存在，则重新添加记录，贡献度从0开始计算
//            DaoDrbStatistics daoDrbStatistics1 = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
//            if (daoDrbStatistics1 != null) {
//                log.info("获取到的daoDrbStatistics1不为空:{}",JacksonUtil.obj2json(daoDrbStatistics1));
//                BeanUtils.copyProperties(daoDrbStatistics1, daoDrbStatistics, "id", "drbNumber", "drbVol",
//                        "drbVolExTax");
//            }
            daoDrbStatistics.setDrbNumber(currentDrb);
            daoDrbStatistics.setStatus(StatisticsStatusEnum.WJS.getStatus());
            daoDrbStatistics.setDaoId(dao.getId());
            //当天零点的时间
            Timestamp today = DateUtil.getBeginOfToday();
            long yesterdayBeginHour = DateUtil.getTimestampAfterDay(today, 0).getTime() / 1000;
            daoDrbStatistics.setRecordTime(yesterdayBeginHour);

        }
        log.info("得到的daoDrbStatistics:{}", JacksonUtil.obj2json(daoDrbStatistics));

        BigDecimal drbVol = daoDrbStatistics.getDrbVol();
        if (drbVol == null) {
            drbVol = BigDecimal.ZERO;
        }
        BigDecimal drbVolExTax = daoDrbStatistics.getDrbVolExTax();
        if (drbVolExTax == null) {
            drbVolExTax = BigDecimal.ZERO;
        }
        daoDrbStatistics.setDrbVol(drbVol.add(work.getMintedPrice()));
        if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
            if (dao.getDaoVersion() >= 1 && dao.getFixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
                daoDrbStatistics.setDrbVolExTax(drbVolExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getDaoMintFee()))));
            } else {
                daoDrbStatistics.setDrbVolExTax(drbVolExTax.add(
                        work.getMintedPrice().multiply(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        } else {
            if (dao.getDaoVersion() >= 1 && dao.getUnfixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
                daoDrbStatistics.setDrbVolExTax(drbVolExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getDaoMintFee()))));
            } else {
                daoDrbStatistics.setDrbVolExTax(drbVolExTax
                        .add(work.getMintedPrice().multiply(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        }
        Integer ownerAmount = workService.selectNftOwners(dao.getId() + "");
        Integer nftAmount = workService.selectNftAmounts(dao.getId() + "");
        Integer workAmount = workService.selectWorkAmounts(dao.getId() + "");
        Integer canvasAmount = canvasService.listCanvasAmountByDaoId(dao.getId() + "");
        Canvas canvas1 = canvasService.listCanvasFloorPriceByDaoId(dao.getId() + "");
        if (canvas1 != null) {
            daoDrbStatistics.setFloorPrice(canvas1.getCurrentPrice());
        }
        daoDrbStatistics.setCanvas(canvasAmount);
        daoDrbStatistics.setOwners(ownerAmount + "");
        daoDrbStatistics.setNft(nftAmount + "");
        daoDrbStatistics.setWorks(workAmount);
        //查询minter Reward 分配比例
        DaoReserveRatio daoReserve;
        if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
            daoReserve = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
        } else {
            daoReserve = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
        }

        log.info("在计算之前获取到的daoDrbStatistics:{}", JacksonUtil.obj2json(daoDrbStatistics));

        //铸造价格等于零的时候按1计数
        BigDecimal contribution = daoDrbStatistics.getContribution() == null ? BigDecimal.ZERO : daoDrbStatistics.getContribution();
        log.info("获取到到contribution:{}", contribution);
        BigDecimal mintPirce = mintedPrice.compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.ONE : mintedPrice;
        log.info("计算得到的mintPirce:{}", mintPirce);
        BigDecimal mintFee = daoReserve.getDaoMintFee().divide(new BigDecimal("100"));
        log.info("计算得到的mintFee-1:{}", mintFee);
        mintFee = mintFee.compareTo(BigDecimal.ZERO) == 0 ? BigDecimal.ONE : mintFee;
        log.info("计算得到的mintFee-2:{}", mintFee);

        daoDrbStatistics.setContribution(contribution.add(mintPirce.multiply(mintFee)));
        log.info("contribution.add(mintPirce.multiply(mintFee))的值为:{}", contribution.add(mintPirce.multiply(mintFee)).toPlainString());

        CanvasDrbStatistics canvasDrbStatistics3 =
                canvasDrbStatisticsService.selectMintRevenueByDaoId(dao.getId(), currentDrb);
        BigDecimal mintRevenue = BigDecimal.ZERO;
        BigDecimal mintRevenueExTax = BigDecimal.ZERO;
        if (canvasDrbStatistics3 != null && canvasDrbStatistics3.getSevenDayDrbVol() != null) {
            mintRevenue = canvasDrbStatistics3.getSevenDayDrbVol();
        }
        if (canvasDrbStatistics3 != null && canvasDrbStatistics3.getMintRevenueExTax() != null) {
            mintRevenueExTax = canvasDrbStatistics3.getMintRevenueExTax();
        }
        log.info("[D4AMintNFTChainService]mintRevenue:{}, daoId:{}", mintRevenue, dao.getId());
        daoDrbStatistics.setMintRevenue(mintRevenue.add(work.getMintedPrice()));
        if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
            if (dao.getDaoVersion() >= 1 && dao.getFixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
                daoDrbStatistics.setMintRevenueExTax(mintRevenueExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getDaoMintFee()))));
            } else {
                daoDrbStatistics.setMintRevenueExTax(mintRevenueExTax.add(
                        work.getMintedPrice().multiply(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }

        } else {
            if (dao.getDaoVersion() >= 1 && dao.getUnfixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
                daoDrbStatistics.setMintRevenueExTax(mintRevenueExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getDaoMintFee()))));
            } else {
                daoDrbStatistics.setMintRevenueExTax(mintRevenueExTax
                        .add(work.getMintedPrice().multiply(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        }

        CanvasDrbStatistics canvasDrbStatistics =
                canvasDrbStatisticsService.selectByCanvasIdAndDrbNumber(canvas.getId(), currentDrb);
        if (canvasDrbStatistics == null) {
            canvasDrbStatistics = new CanvasDrbStatistics();
            CanvasDrbStatistics canvasDrbStatistics1 =
                    canvasDrbStatisticsService.selectLastedByCanvasId(canvas.getId());
            if (canvasDrbStatistics1 != null) {
                BeanUtils.copyProperties(canvasDrbStatistics1, canvasDrbStatistics, "id", "drbNumber", "drbVol",
                        "drbVolExTax", "daoDrbVolExTax");
            }
            canvasDrbStatistics.setCanvasId(canvas.getId());
            canvasDrbStatistics.setDaoId(canvas.getDaoId());
            canvasDrbStatistics.setProjectId(canvas.getProjectId());
            canvasDrbStatistics.setStatus(0);
            canvasDrbStatistics.setDrbNumber(currentDrb);
        }

        BigDecimal canvasDrbVol = canvasDrbStatistics.getDrbVol();
        if (canvasDrbVol == null) {
            canvasDrbVol = BigDecimal.ZERO;
        }
        BigDecimal canvasDrbVolExTax = canvasDrbStatistics.getDrbVolExTax();
        if (canvasDrbVolExTax == null) {
            canvasDrbVolExTax = BigDecimal.ZERO;
        }
        BigDecimal daoDrbVolExTax = canvasDrbStatistics.getDaoDrbVolExTax();
        if (daoDrbVolExTax == null) {
            daoDrbVolExTax = BigDecimal.ZERO;
        }
        canvasDrbStatistics.setDrbVol(canvasDrbVol.add(work.getMintedPrice()));

        // 分配dao erc20信息
        if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {

            if (dao.getDaoVersion() >= 1 && dao.getFixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
                canvasDrbStatistics.setDrbVolExTax(canvasDrbVolExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getCanvasMintFee()))));
                canvasDrbStatistics.setDaoDrbVolExTax(daoDrbVolExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getDaoMintFee()))));
            } else {
                canvasDrbStatistics.setDrbVolExTax(canvasDrbVolExTax.add(work.getMintedPrice()
                        .multiply(BigDecimal.ONE.subtract(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
                                .add(new BigDecimal(ProtoDaoConstant.MINT_D4A_FEE_RATIO)
                                        .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))))));
                canvasDrbStatistics.setDaoDrbVolExTax(daoDrbVolExTax.add(
                        work.getMintedPrice().multiply(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        } else {

            if (dao.getDaoVersion() >= 1 && dao.getUnfixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
                canvasDrbStatistics.setDrbVolExTax(canvasDrbVolExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getCanvasMintFee()))));
                canvasDrbStatistics.setDaoDrbVolExTax(daoDrbVolExTax.add(work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getDaoMintFee()))));
            } else {
                canvasDrbStatistics.setDrbVolExTax(canvasDrbVolExTax.add(work.getMintedPrice()
                        .multiply(BigDecimal.ONE.subtract(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
                                .add(new BigDecimal(ProtoDaoConstant.MINT_D4A_FEE_RATIO)
                                        .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))))));
                canvasDrbStatistics.setDaoDrbVolExTax(daoDrbVolExTax
                        .add(work.getMintedPrice().multiply(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        }

        Integer cOwnerAmount = workService.selectNftOwnersByCanvasId(canvas.getCanvasId());
        Integer cNftAmount = workService.selectNftAmountsByCanvasId(canvas.getCanvasId());
        Integer cWorkAmount = workService.selectWorkAmountsByCanvasId(canvas.getCanvasId());
        canvasDrbStatistics.setOwners(cOwnerAmount);
        canvasDrbStatistics.setNft(cNftAmount);
        canvasDrbStatistics.setWorkAmount(cWorkAmount);

        CanvasDrbStatistics canvasDrbStatistics2 =
                canvasDrbStatisticsService.selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", 0, currentDrb - 1);
        if (canvasDrbStatistics2 != null && canvasDrbStatistics2.getSevenDayDrbVol() != null) {
            canvasDrbStatistics
                    .setMintRevenue(canvasDrbStatistics2.getSevenDayDrbVol().add(canvasDrbStatistics.getDrbVol()));
            canvasDrbStatistics
                    .setTotalVol(canvasDrbStatistics2.getSevenDayDrbVol().add(canvasDrbStatistics.getDrbVol()));
        } else {
            canvasDrbStatistics.setMintRevenue(canvasDrbStatistics.getDrbVol());
            canvasDrbStatistics.setTotalVol(canvasDrbStatistics.getDrbVol());
        }

        if (canvasDrbStatistics2 != null && canvasDrbStatistics2.getMintRevenueExTax() != null) {
            canvasDrbStatistics.setMintRevenueExTax(
                    canvasDrbStatistics2.getMintRevenueExTax().add(canvasDrbStatistics.getDrbVolExTax()));

        } else {
            canvasDrbStatistics.setMintRevenueExTax(canvasDrbStatistics.getDrbVolExTax());
        }

        Integer startDrb = currentDrb >= 6 ? currentDrb - 6 : 0;
        BigDecimal canvasSevenDayDrbVol;
        CanvasDrbStatistics canvasDrbStatistics1 =
                canvasDrbStatisticsService.selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", startDrb, currentDrb);
        if (canvasDrbStatistics1 != null && canvasDrbStatistics1.getSevenDayDrbVol() != null) {
            canvasSevenDayDrbVol = canvasDrbStatistics1.getSevenDayDrbVol().add(work.getMintedPrice());
            canvasDrbStatistics.setSevenDayDrbVol(canvasSevenDayDrbVol);
        } else {
            canvasSevenDayDrbVol = canvasDrbStatistics.getDrbVol();
            canvasDrbStatistics.setSevenDayDrbVol(canvasSevenDayDrbVol);
        }

        if (daoDrbStatistics.getDrbVol().compareTo(BigDecimal.ZERO) > 0) {
            canvasDrbStatistics
                    .setNtvr(canvasDrbStatistics.getDrbVol().divide(daoDrbStatistics.getDrbVol(), 4, RoundingMode.FLOOR));
        } else {
            canvasDrbStatistics.setNtvr(BigDecimal.ZERO);
        }


        List<DaoDrbStatistics> daoDrbStatisticsList1 = daoDrbStatisticsService.selectGalleryDao(startDrb, currentDrb);
        Integer daoId = canvasDrbStatistics.getDaoId();
        BigDecimal sevenDayDrbVol = daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(daoId))
                .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        sevenDayDrbVol = sevenDayDrbVol.add(work.getMintedPrice());
        if (sevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0) {
            canvasDrbStatistics.setSevenDayNtrv(canvasSevenDayDrbVol.divide(sevenDayDrbVol, 4, RoundingMode.FLOOR));
        } else {
            canvasDrbStatistics.setSevenDayNtrv(BigDecimal.ZERO);
        }

        daoDrbStatistics.setSevenDayDrbVol(sevenDayDrbVol);

        // canvas next price
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.CANVAS_NEXT_PRICE.getContractAddress());
        infuraCallRequestDto.setData(ContractMethodEnum.CANVAS_NEXT_PRICE.getMethodAddress() + canvasId);

        // 调用查询使用数据集的user
        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[NewCanvasChainService] error result:{} transactionDto:{}", result.getResultDesc(),
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("保存work查询canvas价格信息失败");
        }
        log.info("[NewCanvasChainService]infura return data:{}", result.getData());
        String canvasInfoData = result.getData();
        String canvasPrice = CommonUtil.hexToTenString(canvasInfoData);
        BigDecimal currentPrice = BigDecimal.ZERO;//new BigDecimal(canvasPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO));
        if (StringUtils.isNotBlank(canvasPrice)) {
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
                currentPrice = new BigDecimal(canvasPrice).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals()));
            } else {
                currentPrice = new BigDecimal(canvasPrice).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()));
            }
        }
        canvas.setCurrentPrice(currentPrice);
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            canvas.setCurrentPrice(dao.getGlobalDaoPrice());
        }

        canvasDrbStatistics.setMintPrice(currentPrice);

        canvasService.updateById(canvas);

        log.info("[D4AMintNFTChainService]currentPrice:{} daoFloorPrice:{}", currentPrice.toPlainString(),
                dao.getDaoFloorPrice().toPlainString());
        if (!WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())
                && mintedPrice.compareTo(dao.getDaoFloorPrice()) < 0) {
            log.info("[D4AMintNFTChainService]currentPrice equals daoFloorPrice");
            List<Canvas> canvasList = canvasService.listCanvasByDaoId(dao.getId() + "");
            BigDecimal currentPriceDecimal = currentPrice;
            canvasList.forEach(v -> v.setCurrentPrice(currentPriceDecimal));
            canvasService.updateBatchById(canvasList);

            daoDrbStatistics.setFloorPrice(currentPrice);
        }

        if (nftAmount.compareTo(dao.getTotalNftCasting()) >= 0) {
            dao.setDaoStatus(DaoStatusEnum.FINISHED.getStatus());
            List<Canvas> canvasList = canvasService.listCanvasByDaoId(dao.getId() + "");
            canvasList.forEach(v -> v.setDaoStatus(DaoStatusEnum.FINISHED.getStatus()));
            canvasService.updateBatchById(canvasList);
        }
        if (daoDrbStatistics.getFloorPrice() != null) {
            dao.setCanvasFloorPrice(daoDrbStatistics.getFloorPrice());
        }
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            dao.setCanvasFloorPrice(dao.getGlobalDaoPrice());
        }
        if (daoDrbStatistics.getFloorPrice() != null || nftAmount.compareTo(dao.getTotalNftCasting()) >= 0) {
            daoService.updateById(dao);
        }
        // 当某一个的铸造价格为地板价时，所有canvas的价格都恢复为地板价
//        if (!Integer.valueOf(1).equals(dao.getTopupMode())) {
        //1.4 当前window分配的token和eth计算
        List<DaoAllocationStrategy> daoAllocationStrategies = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);
        DaoAllocationStrategy optional = new DaoAllocationStrategy();
        DaoAllocationStrategy daoAllocationStrategyToken = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.FALSE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.THREE.getType().equals(v.getRoyaltyType())).findFirst().orElse(optional);
        DaoAllocationStrategy daoAllocationStrategyEth = daoAllocationStrategies.stream().filter(v -> TrueOrFalseEnum.TRUE.getStatus().equals(v.getType()) && DaoRoyaltyTypeEnum.THREE.getType().equals(v.getRoyaltyType())).findFirst().orElse(optional);

        BigDecimal royaltyProportion = daoAllocationStrategyToken.getRoyaltyProportion() == null ? BigDecimal.ZERO : daoAllocationStrategyToken.getRoyaltyProportion();
        BigDecimal ethRoyaltyProportion = daoAllocationStrategyEth.getRoyaltyProportion() == null ? BigDecimal.ZERO : daoAllocationStrategyEth.getRoyaltyProportion();
        daoDrbStatistics.setAssetPoolTokenCost(commonService.getRoundErc20Reward(dao, Integer.valueOf(dao.getCurrentRound())).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(new BigDecimal("100").subtract(royaltyProportion))));
        daoDrbStatistics.setAssetPoolEthCost(commonService.getRoundEthReward(dao, Integer.valueOf(dao.getCurrentRound())).multiply(ProtoDaoCommonUtil.bigdecimalPercentage(new BigDecimal("100").subtract(ethRoyaltyProportion))));


        // 生成canvas create 的奖励

        addTokenReceivedRecord(dao, work, transactionDto, canvas.getOwnerAddress());


        canvasDrbStatisticsService.saveOrUpdate(canvasDrbStatistics);
        daoDrbStatisticsService.saveOrUpdate(daoDrbStatistics);
//        }


        int generateWorkNumber = dao.getGenerateWorkSet() > 0 ? dao.getGenerateWorkSet() : ProtoDaoConstant.AUTO_GENERATE_WORK_NUMBER;
        //生成下一个work
        if (dao.getNeedMintableWork() == 0 && Integer.parseInt(token_id) < generateWorkNumber) {
            Integer workNum = Integer.parseInt(token_id) + 1;
            boolean addWork = commonService.addWork(dao, canvas, workNum);
            log.info("[D4AMintNFTChainService] addWork daoId:{} workNum:{} result:{}", dao.getId(), workNum, addWork);
            if (!addWork) {
                log.error("[D4AMintNFTChainService] addWork fail daoId:{}", dao.getId());
                dao.setAddWork(0);
                daoService.updateById(dao);
            }
        }

        //1.3 dao topup模式更新铸造者的token和eth的数量
//        if (WorkTopupModeEnum.MINT_TOPUP_NO.getStatus().equals(work.getTopupMode()) || WorkTopupModeEnum.MINT_TOPUP_YES.getStatus().equals(work.getTopupMode())) {
        String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        //commonService.updateMinterTopupHarvest(existDaoId, work.getMintedAddress());

        // 1.5 绑定在 work而不是账户
        if (!ProtoDaoConstant.ZERO_ADDRESS.equals(mount_dao_721tokenAddress)) {
            Dao mountDao = daoService.selectDaoByErc721Token(mount_dao_721tokenAddress);
            Work mountWork = workService.selectWorkByNumber(mountDao.getId(), mount_work_number);

            // 更新work表绑定的信息
            work.setMountWorkId(mountWork.getId());
            workService.updateById(work);

            commonService.updateMinterWorkTopupHarvest(existDaoId, work.getId(), mountDao, mountWork);
        }

//        }

    }

    private void addTokenReceivedRecord(Dao dao, Work work, TransactionDto transactionDto, String toAddress) {
        log.info("[D4AMintNFTChainService] addTokenReceivedRecord daoId:{} workId:{}", dao.getId(), work.getId());
        BigDecimal tokenNum;

        if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
            if (dao.getDaoVersion() >= 1 && dao.getFixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
                tokenNum = work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getCanvasMintFee()));
            } else {
                tokenNum = work.getMintedPrice()
                        .multiply(BigDecimal.ONE.subtract(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
                                .add(new BigDecimal(ProtoDaoConstant.MINT_D4A_FEE_RATIO)
                                        .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        } else {
            if (dao.getDaoVersion() >= 1 && dao.getUnfixedReserveRatio() != null) {
                DaoReserveRatio daoReserveRatio =
                        JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
                tokenNum = work.getMintedPrice()
                        .multiply(ProtoDaoCommonUtil.bigdecimalPercentage(daoReserveRatio.getCanvasMintFee()));
            } else {
                tokenNum = work.getMintedPrice()
                        .multiply(BigDecimal.ONE.subtract(new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO)
                                .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
                                .add(new BigDecimal(ProtoDaoConstant.MINT_D4A_FEE_RATIO)
                                        .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)))));
            }
        }


        TokenReceivedRecord tokenReceivedRecord = new TokenReceivedRecord();
        tokenReceivedRecord.setTokenNum(tokenNum);
        tokenReceivedRecord.setTokenNumBalance(tokenNum);
        tokenReceivedRecord.setBlockNumber(transactionDto.getBlockNumber());
        tokenReceivedRecord.setBlockTime(transactionDto.getBlockTime());
        tokenReceivedRecord.setTransactionHash(transactionDto.getTransactionHash());
        tokenReceivedRecord.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
        tokenReceivedRecord.setFromAddress(ProtoDaoConstant.protocolContract);
        tokenReceivedRecord.setToAddress(toAddress);
        tokenReceivedRecord.setTokenType(TokenTypeEnum.TRANSFER.getType());
        tokenReceivedRecord.setReceiveType(TokenReceiveTypeEnum.MINTER.getType());

        tokenReceivedRecord.setDaoNumber(dao.getDaoNumber());
        tokenReceivedRecord.setReceiveId(dao.getId());
        tokenReceivedRecord.setReceiveAddress(toAddress);
        tokenReceivedRecord.setProjectId(dao.getProjectId());

        tokenReceivedRecordService.save(tokenReceivedRecord);
    }
}
