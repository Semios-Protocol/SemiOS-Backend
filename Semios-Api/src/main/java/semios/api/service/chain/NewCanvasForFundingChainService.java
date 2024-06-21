package semios.api.service.chain;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.response.NewCanvasUriDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.ContractMethodEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.ImageUtil;
import semios.api.utils.JacksonUtil;

import java.io.File;
import java.math.BigDecimal;
import java.util.List;

/**
 * 1.3 NewCanvasForFunding
 *
 * @description: 监听创建canvas
 * @author: xiangbin
 * @create: 2022-08-24 18:14
 **/
@Slf4j
@Service
public class NewCanvasForFundingChainService implements SubscriberChainService {

    private static final RestTemplate restTemplate = new RestTemplate();
    @Autowired
    private ICanvasService canvasService;
    @Autowired
    private IDaoService daoService;
    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;
    @Autowired
    private IDaoDrbStatisticsService daoDrbStatisticsService;
    @Value("${dao_default_logo}")
    private String daoDefaultLogo;
    @Value("${file.dir}")
    private String fileDir;
    @Value("${work_image_url}")
    private String workImageUrl;
    @Autowired
    private S3Service s3Service;
    @Autowired
    private IWorkService workService;
    @Autowired
    private CommonService commonService;

    public static void main(String[] args) {

        String data =
                "86c28aafc0def9ece950e21fac59f4ee28a3b5d36b2cd07a1fcc3e86d36e3adb33a4678c1010c8e45ec3ecf520db00f2abb596f337bde44265de6408961fa39d0000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000008768747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f63616e7661732f333361343637386331303130633865343565633365636635323064623030663261626235393666333337626465343432363564653634303839363166613339642e6a736f6e00000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String uri = CommonUtil.dynamicArgumentDecoding(data, dataList.get(2), true);

        System.out.println("123");

//        /**
//         * data:[ ['2023.04.19 12:01:01',50], ['2023.04.19 17:01:01',20] ]
//         */
//        SimpleDateFormat sdf = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss");
//        List<List<String>> stringList = new ArrayList<>();
//        LocalDate dateTime = LocalDate.of(2023, 4, 12);
//        Random random = new Random();
//        for (int i = 0; i <= 7; i++) {
//            dateTime = dateTime.plusDays(1);
//            for (int j = 0; j < 3; j++) {
//                List<String> array = new ArrayList<>();
//                int m = random.nextInt(50);
//                int n = random.nextInt(960);
//
//                LocalDateTime localDateTime = dateTime.atStartOfDay().plusMinutes(n);
//                // ZonedDateTime zonedDateTime = localDateTime.atZone(ZoneId.systemDefault());
//                // Instant instant2 = zonedDateTime.toInstant();
//                // Date date2 = Date.from(instant2);
//                array.add(localDateTime.toInstant(ZoneOffset.of("+8")).getEpochSecond() + "");
//                // array.add(sdf.format(date2));
//                array.add(m + "");
//                stringList.add(array);
//            }
//        }
//        System.out.println(JacksonUtil.obj2json(stringList));
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[NewCanvasForFundingChainService]transactionDto:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String uri = CommonUtil.dynamicArgumentDecoding(data, dataList.get(2), true);

        Canvas canvas = canvasService.selectCanvasByUri(uri);
        if (canvas != null) {
            log.error("[NewCanvasForFundingChainService] canvas uri:{} is exist transactionDto:{}", uri,
                    JacksonUtil.obj2json(transactionDto));
            return;
        }

        NewCanvasUriDto newCanvasUriDto = restTemplate.getForObject(uri, NewCanvasUriDto.class);
        log.info("[NewCanvasForFundingChainService]uri return canvasInfo:{}", JacksonUtil.obj2json(newCanvasUriDto));
        if (newCanvasUriDto == null) {
            log.error("[NewCanvasForFundingChainService] canvas uri:{} is null transactionDto:{}", uri,
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("uri is not available");
        }

        // canvas next price
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.CANVAS_NEXT_PRICE.getContractAddress());
        infuraCallRequestDto.setData(ContractMethodEnum.CANVAS_NEXT_PRICE.getMethodAddress() + canvasId);

        Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[NewCanvasForFundingChainService] error result:{} transactionDto:{}", result.getResultDesc(),
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("保存canvas查询价格信息失败");
        }
        log.info("[NewCanvasForFundingChainService]infura return data:{}", result.getData());
        String canvasInfoData = result.getData();
        String price = CommonUtil.hexToTenString(canvasInfoData);

        // 查询canvas index
        InfuraCallRequestDto indexRequest = new InfuraCallRequestDto();
        indexRequest.setNetWork(ProtoDaoConstant.netWork);
        indexRequest.setTo(ContractMethodEnum.CANVAS_INDEX.getContractAddress());
        indexRequest.setData(ContractMethodEnum.CANVAS_INDEX.getMethodAddress() + canvasId);

        Result<String> indexResult = iSubscriptionService.infuraCall(indexRequest);
        if (indexResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[NewCanvasForFundingChainService] index error result:{}", indexResult.getResultDesc());
            throw new RuntimeException("保存canvas查询index信息失败");
        }
        log.info("[NewCanvasForFundingChainService]infura index return data:{}", indexResult.getData());
        String indexResultData = indexResult.getData();
        String canvasNumber = CommonUtil.hexToTenString(indexResultData);

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[NewCanvasForFundingChainService] daoDetailByProjectId error dao is null");
            throw new RuntimeException("保存canvas查询dao为空");
        }
        Page<DaoDrbStatistics> iPage = new Page<>(1, 10);
        Page<DaoDrbStatistics> daoDrbStatisticsPage = daoDrbStatisticsService.selectByDaoId(iPage, dao.getId());
        long blockNumber = daoDrbStatisticsPage.getTotal();

        canvas = new Canvas();

        canvas.setDrbNumber(Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
        canvas.setTotalDrbNumber(dao.getDaoMintWindow() - Integer.parseInt(String.valueOf(blockNumber)));
        canvas.setCanvasName(newCanvasUriDto.getName());
        canvas.setCanvasDescription(newCanvasUriDto.getDescription());
        if (StringUtils.isNotBlank(newCanvasUriDto.getLogo())) {
            canvas.setCanvasLogo(newCanvasUriDto.getLogo());
        } else {
            canvas.setCanvasLogo(daoDefaultLogo);
        }
        canvas.setSocialLinks(newCanvasUriDto.getSocialLinks());
        if (StringUtils.isNotBlank(canvasNumber)) {
            canvas.setCanvasNumber(Integer.valueOf(canvasNumber));
        }
        canvas.setDaoId(dao.getId());
        canvas.setProjectId(projectId);
        canvas.setDaoNumber(dao.getDaoNumber());
        canvas.setDaoSymbol(dao.getDaoSymbol());
        canvas.setTransactionHash(transactionDto.getTransactionHash());
        canvas.setBlockTime(transactionDto.getBlockTime());
        if (StringUtils.isNotBlank(price)) {
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
                canvas.setCurrentPrice(new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())));
            } else {
                canvas.setCurrentPrice(new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
            }
        }
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            canvas.setCurrentPrice(dao.getGlobalDaoPrice());
        }
        canvas.setBlockNumber(transactionDto.getBlockNumber());
        canvas.setCanvasId(canvasId);
        canvas.setDaoFloorPrice(dao.getDaoFloorPrice());
        canvas.setCanvasUri(uri);
        canvas.setReceivedToken(BigDecimal.ZERO);
        canvas.setUnclaimedToken(BigDecimal.ZERO);
        canvas.setRestDrb(0);
        canvas.setCanvasStatus(CanvasStatusEnum.CREATED.getStatus());

        canvas.setCanvasName(newCanvasUriDto.getName());
        canvas.setCanvasDescription(newCanvasUriDto.getDescription());
        // canvas.setCanvasLogo(newCanvasUriDto.getLogo());
        canvas.setCanvasUri(uri);

        canvas.setDaoId(dao.getId());
        canvas.setProjectId(dao.getProjectId());
        canvas.setDaoNumber(dao.getDaoNumber());
        canvas.setDaoSymbol(dao.getDaoSymbol());
        canvas.setOwnerAddress(newCanvasUriDto.getUser_address());
        canvas.setOpenseaLink(newCanvasUriDto.getOpensea_link());
        canvas.setTwitterLink(newCanvasUriDto.getTwitter_link());
        canvas.setDiscordLink(newCanvasUriDto.getDiscord_link());

        canvasService.save(canvas);

        //首次创建canvas的时候创建work，之后每次铸造之后添加一个。
        if (canvas.getOwnerAddress().equals(dao.getOwnerAddress())) {

            //处理logo转work图片地址及高度／背景色 hash等信息 抽象为一个方法
            String filePathDir = String.format(ProtoDaoConstant.workImageDaoLogoUrl, dao.getDaoNumber());
            File imageFile = new File(filePathDir);
            if (!imageFile.exists()) {
                imageFile.mkdirs();
            }
            String filePath = String.format(ProtoDaoConstant.workImageDaoLogoUrl, dao.getDaoNumber()) + File.separatorChar + "0.png";
            imageFile = new File(filePath);

            ImageUtil.imageAddText(ProtoDaoConstant.workImageDefaultUrl, filePath, dao.getDaoName(), null, "png");

            s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.daoBucketName, imageFile,
                    dao.getDaoNumber() + "_0.png", false);

            Dao updateDao = new Dao();
            updateDao.setId(dao.getId());

            updateDao.setWorkUrlSuffix(".png");
            ImageUtil.HeightAndBgColor heightAndBgColor = ImageUtil.getImageRgb(imageFile);
            if (heightAndBgColor != null) {
                updateDao.setHeight(heightAndBgColor.getHeight());
                updateDao.setColor(heightAndBgColor.getBgColor());
            }
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            String daoLogoWorkName = dao.getDaoNumber() + "_0.png";
            String daoWorkUrl = urlPrefix + ProtoDaoConstant.daoBucketName + "/" + daoLogoWorkName;
            updateDao.setDaoWorkUrl(daoWorkUrl);
            String workHash = ImageUtil.getMD5(imageFile);
            updateDao.setWorkHash(workHash);
            daoService.updateById(updateDao);

            if (dao.getNeedMintableWork() == 0 && DaoStatusEnum.STARTED.getStatus().equals(dao.getDaoStatus())) {
                //创建1000个work 改为铸造一个创建一个的方式
                boolean addWork = commonService.addWork(dao, canvas, 1);
                log.info("[NewCanvasForFundingChainService] addWork daoId:{} workNum:{} result:{}", dao.getId(), 1, addWork);
                if (addWork) {
                    log.info("[NewCanvasForFundingChainService] addWork success daoId:{}", dao.getId());
                } else {
                    log.error("[NewCanvasForFundingChainService] addWork fail daoId:{}", dao.getId());
                    updateDao.setAddWork(0);
                    daoService.updateById(updateDao);
                }
            }

        }

    }

}
