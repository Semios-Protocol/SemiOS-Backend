package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.MintWorkUriDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.NodePermissionNft;
import semios.api.model.entity.Work;
import semios.api.model.enums.*;
import semios.api.model.vo.req.WorkCreateReqVo;
import semios.api.service.*;
import semios.api.utils.CommonUtil;
import semios.api.utils.ImageUtil;
import semios.api.utils.JacksonUtil;

import javax.annotation.Resource;
import java.io.File;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/*
 * 创建dao给sub dao 打款生成的erc721地址
 * bytes32 daoId,  //具体dao project id
 * address daoNft, //dao原erc721的地址
 * address grantDaoNft // 打款赠送的erc721的地址
 * */
@Slf4j
@Service
public class NewSemiDaoErc721AddressChainService implements SubscriberChainService {

    private static final RestTemplate restTemplate = new RestTemplate();
    @Resource
    private IDaoService daoService;
    @Resource
    private ICanvasService canvasService;
    @Resource
    private INodePermissionNftService nodePermissionNftService;
    @Value("${work_image_url}")
    private String nftImageUrl;
    @Autowired
    private IWorkService workService;
    @Autowired
    private S3Service s3Service;
    private final ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewSemiDaoErc721AddressChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0)); //dao的project id
        String daoNftErc721 = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))); //dao原erc721的地址
        String grantDaoNftErc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2))); //用于给sub dao打款赠送的erc721的地址

        Dao dao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());
        if (dao == null) {
            log.error("[NewSemiDaoErc721AddressChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiDaoErc721AddressChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setDaoNftErc721(daoNftErc721);
        updateDao.setGrantDaoNftErc721(grantDaoNftErc721Address);

        daoService.updateById(updateDao);


        // 创建0号NFT
        Canvas canvas = canvasService.selectCanvasByTransactionHash(dao.getTransactionHash());
        if (canvas == null) {
            log.error("[NewSemiDaoErc721AddressChainService] canvas is null,add zero nft:dao id:{},transaction has:{}", dao.getId(), dao.getTransactionHash());
            throw new RuntimeException("NewSemiDaoErc721AddressChainService cannot find canvas");
        }

        boolean isCreateZeroNft = createZeroNft(dao, canvas, transactionDto);
        if (!isCreateZeroNft) {
            log.error("[NewSemiDaoErc721AddressChainService] add zero nft is error:dao id:{},transaction has:{}", dao.getId(), dao.getTransactionHash());
            throw new RuntimeException("NewSemiDaoErc721AddressChainService add zero nft is error");
        }
        // 添加所有的权限
        boolean isSaveNodePermission = insertNodePermission(dao);// sub node
        if (!isSaveNodePermission) {
            log.error("[NewSemiDaoErc721AddressChainService] add node permission is error:dao id:{},transaction has:{}", dao.getId(), dao.getTransactionHash());
            throw new RuntimeException("NewSemiDaoErc721AddressChainService add zero nft is error");
        }
        log.info("[NewSemiDaoErc721AddressChainService] over:{}", dao.getProjectId());


        // 30秒后执行
        executorService.schedule(() -> {
            // 刷新oponsea名称
            String openseaUrl =
                    String.format(ProtoDaoConstant.openseaApiNftLink, dao.getErc721Token(), 0);
            log.info("[NewSemiDaoErc721AddressChainService]openseaUrl:{}", openseaUrl);
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
                log.info("[NewSemiDaoErc721AddressChainService]response:{}", response);
            } catch (Exception e) {
                log.error("[NewSemiDaoErc721AddressChainService] fresh openseaUrl:{} error:{}", openseaUrl, e);
            }
        }, 30, TimeUnit.SECONDS);

    }

    // 添加0号nft
    private Boolean createZeroNft(Dao dao, Canvas canvas, TransactionDto transactionDto) {
        try {
            //上传 图片
            String imageFileNameAws = "0-nft.png";
            String dirStr = String.format(nftImageUrl, dao.getDaoNumber());
            File dirPath = new File(dirStr);
            if (!dirPath.exists()) {
                dirPath.mkdirs();
            }
            // 创建目录，并在目录下创建0-nft.png的文件
            String filePath = dirStr + File.separator + imageFileNameAws;
            File imageFile = new File(filePath);

            //图片添加文字,并写入到刚刚创建的0-nft.png
            String nftImageDefaultUrl = ProtoDaoConstant.nftImageDefaultUrl;
            ImageUtil.imageAddTextNft(nftImageDefaultUrl, filePath, dao.getDaoName(), "png");

            String workHash = ImageUtil.getMD5(imageFile);

            Work work = new Work();
            ImageUtil.HeightAndBgColor heightAndBgColor = ImageUtil.getImageRgb(imageFile);
            if (heightAndBgColor != null) {
                log.info("daoNumber:{} workNumber:{} height:{} bgColor:{}", dao.getDaoNumber(), 0, heightAndBgColor.getHeight(),
                        heightAndBgColor.getBgColor());
                work.setHeight(heightAndBgColor.getHeight());
                work.setBgColor(heightAndBgColor.getBgColor());
            } else {
                log.error("daoNumber:{} workNumber:{} image not height error", dao.getDaoNumber(), 0);
                return false;
            }

            String imageUrl = "";
            //上传json文件
            String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
            try {
                // 文件上传前的名称 处理图片用
                s3Service.putImage(ProtoDaoConstant.bucketName + ProtoDaoConstant.workBucketName + "/" + dao.getDaoNumber(), imageFile, imageFileNameAws, true);

                imageUrl = urlPrefix + ProtoDaoConstant.workBucketName + "/" + dao.getDaoNumber() + "/" + imageFileNameAws;
                log.info("[work-create] s3DaoLogoUrl:{}", imageUrl);
            } catch (Exception e) {
                log.error("[work-create]i:{} upload image error", 0, e);
                return false;
            }

            // 处理uri用
            WorkCreateReqVo workCreateReqVo = new WorkCreateReqVo();
            workCreateReqVo.setImageUrl(imageUrl);
            MintWorkUriDto mintWorkUriDto = MintWorkUriDto.transfer(workCreateReqVo);
            String imageName = dao.getDaoNumber() + "-" + 0;
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
                log.error("[work-create]i:{} upload image error", 0, e);
                return false;
            }

            // 根据是否是main dao来判断描述信息
            work.setWorkDescription(ZeroNftDescriptionEnum.getDescByIsAncestordao(dao.getIsAncestordao()));
            work.setWorkStatus(WorkStatusEnum.CASTED.getStatus());  // 设置为已铸造
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
            work.setWorkNumber(0);
            work.setCreateSignHash(""); //空串
            work.setCreatorAddress(dao.getOwnerAddress());
            work.setOwnerAddress(dao.getOwnerAddress());
            work.setGenerate(3);
            // 一口价 0.01
            work.setPriceType(WorkPriceTypeEnum.FIXED_PRICE.getType());
            if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                work.setFixedPrice(dao.getGlobalDaoPrice());
            } else {
                work.setFixedPrice(dao.getDaoFloorPrice());
            }
            work.setMintedPrice(BigDecimal.ZERO);
            work.setTransactionHash(dao.getTransactionHash());
            work.setBlockNumber(transactionDto.getBlockNumber());
            work.setBlockTime(transactionDto.getBlockTime());
            work.setMintedAddress(dao.getOwnerAddress());
            work.setDrbNumber(Integer.parseInt(dao.getCurrentRound()));


            return workService.save(work);
        } catch (Exception e) {
            log.error("daoNumber:{} workNumber:{} exception:", dao.getDaoNumber(), 0, e);
        }
        return false;

    }


    // dao 为sub node
    private Boolean insertNodePermission(Dao dao) {
        // 如果已经存在了，并且交易hash一致，不用重复生成
        NodePermissionNft nodePermissionInfo = nodePermissionNftService.selectNodePermissionNft(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()),
                NodePermissionTypeEnum.Edit_SubNode.getType());
        if (nodePermissionInfo != null && nodePermissionInfo.getTransactionHash().equalsIgnoreCase(dao.getTransactionHash())) {
            log.info("[insertNodePermission] dao id:{} transaction:{} nodePermissionInfo is exist", dao.getId(), dao.getTransactionHash());
            return true;
        }

        Work work = workService.selectWorkByNumber(dao.getId(), "0");
        if (work == null) {
            log.error("[insertNodePermission] dao id():{} workNumber:{} work is null", dao.getId(), 0);
            return false;
        }

        List<NodePermissionNft> nodePermissionNftList = new ArrayList<>();

        for (int i = 1; i <= 4; i++) {
            NodePermissionNft nodePermissionNft = createNodePermissionNft(dao, work);
            nodePermissionNft.setDaoId(dao.getId());
            nodePermissionNft.setNodeType(2); // sub node
            nodePermissionNft.setPermissionsType(i);

            nodePermissionNftList.add(nodePermissionNft);
        }

        // 插入聚合dao权限信息
        Dao togetherDao = daoService.getDaoByProjectId(dao.getProjectId(), DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus());
        if (togetherDao != null) {
            for (int i = 5; i <= 7; i++) {
                NodePermissionNft nodePermissionNft = createNodePermissionNft(dao, work);
                nodePermissionNft.setDaoId(togetherDao.getId());
                nodePermissionNft.setNodeType(1); // seed node
                nodePermissionNft.setPermissionsType(i);

                nodePermissionNftList.add(nodePermissionNft);
            }
        }

        return nodePermissionNftService.saveBatch(nodePermissionNftList);
    }

    // dao传值为sub node
    // 聚合dao 在外层后重新赋值
    private NodePermissionNft createNodePermissionNft(Dao dao, Work work) {
        NodePermissionNft nodePermissionNft = new NodePermissionNft();

        nodePermissionNft.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        nodePermissionNft.setGenerateWorkId(work.getId());
        nodePermissionNft.setGenerateErc721Address(dao.getErc721Token());
        nodePermissionNft.setGenerateErc721TokenId("0"); // 0号NFT
        nodePermissionNft.setPermissionsDaoId(work.getDaoId());
        nodePermissionNft.setPermissionsWorkId(work.getId());
        nodePermissionNft.setPermissionsErc721Address(dao.getErc721Token());
        nodePermissionNft.setPermissionsErc721TokenId("0");
        nodePermissionNft.setTransactionHash(dao.getTransactionHash());
        return nodePermissionNft;
    }
}
