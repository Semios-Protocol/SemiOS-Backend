package semios.api.model.vo.res;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.Data;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.utils.CodeUtil;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @description: 创建work时返回的关于canvas信息
 * @author: xiangbin
 * @create: 2022-08-11 15:04
 **/
@Data
public class CanvasForCreateWorkResVo {

    /**
     * 未铸造的三个work的图片地址
     */
    private List<String> unmintedWorkUrls;

    /**
     * canvas下未铸造work数量
     */
    private Integer unmintedWorkAmount;

    /**
     * canvas下未铸造work总量限制 默认500
     */
    private Integer unmintedWorkTotal = 500;

    /**
     * dao设置的创建手续费
     */
    private Double daoCreatorFee;

    /**
     * dao设置的地板价 为0时 代表地板价设置的0价格 当地板价为0ETH时，用户不能设置一口价的作品
     */
    private Double daoFloorPrice;

    /**
     * canvas当前铸造价格
     */
    private Double canvasCurrentPrice;

    /**
     * protocol 合约地址
     */
    private String protocolContract = ProtoDaoConstant.protocolContract;

    /**
     * work uri地址信息
     */
    private String workUri;

    /**
     * work uri 随机数
     */
    private String workUriRandom;

    /**
     * canvasId
     */
    private String canId;


    /**
     * D4A 铸造费用 version_1.5
     *
     * @mock 0.025
     */
    private String d4aMintFee =
            new BigDecimal(ProtoDaoConstant.MINT_D4A_FEE_RATIO).divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
                    .multiply(new BigDecimal("100")).stripTrailingZeros().toPlainString();

    /**
     * Dao 铸造费用 version_1.5
     *
     * @mock 0.3
     */
    private String daoMintFee =
            new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO).divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE))
                    .multiply(new BigDecimal("100")).stripTrailingZeros().toPlainString();

    /**
     * Dao 一口价铸造费用 version_1.5
     *
     * @mock 0.35
     */
    private String fixedPricedaoMintFee = new BigDecimal(ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE)
            .divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)).multiply(new BigDecimal("100")).stripTrailingZeros()
            .toPlainString();

    /**
     * canvas设置的值
     */
    private BigDecimal royaltyToken = BigDecimal.ZERO;

    /**
     * dao版本 3
     */
    private Integer daoVersion;


    /**
     * Twitter链接地址
     */
    private String twitterLink;

    /**
     * Discord链接地址
     */
    private String discordLink;

    /**
     * 非一口价铸造分配比例
     */
    private DaoReserveRatio daoReserveRatio = new DaoReserveRatio();

    /**
     * 一口价铸造分配比例
     */
    private DaoReserveRatio fixedDaoReserveRatio = new DaoReserveRatio(true);

    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     */
    private Integer basicDao;

    /**
     * 是否为unifiedPrice价格
     */
    private Boolean unifiedPrice = false;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    /**
     * dao erc20 address
     */
    private String daoErc20Address;


    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * dao symbol
     */
    private String daoSymbol;

    public static CanvasForCreateWorkResVo transfer(Dao dao, Canvas canvas, Page<Work> workPage) {
        List<String> workPictures = workPage.getRecords().stream().map(Work::getImageUrl).collect(Collectors.toList());
        // int size = workPictures.size();
        // while(size < 3){
        // workPictures.add(canvas.getCanvasLogo());
        // size++;
        // }
        CanvasForCreateWorkResVo canvasForCreateWorkResVo = new CanvasForCreateWorkResVo();
        canvasForCreateWorkResVo.setUnmintedWorkUrls(workPictures);
        canvasForCreateWorkResVo.setUnmintedWorkAmount(Long.valueOf(workPage.getTotal()).intValue());

//        canvasForCreateWorkResVo.setDaoCreatorFee(dao.getDaoCreateFee().doubleValue());

        canvasForCreateWorkResVo.setDaoFloorPrice(dao.getDaoFloorPrice().doubleValue());
        canvasForCreateWorkResVo.setDaoVersion(dao.getDaoVersion());
        canvasForCreateWorkResVo.setCanvasCurrentPrice(canvas.getCurrentPrice().doubleValue());
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            canvasForCreateWorkResVo.setUnifiedPrice(true);
            canvasForCreateWorkResVo.setCanvasCurrentPrice(dao.getGlobalDaoPrice().doubleValue());
        }
        canvasForCreateWorkResVo.setTwitterLink(dao.getTwitterLink());
        canvasForCreateWorkResVo.setDiscordLink(dao.getDiscordLink());
        canvasForCreateWorkResVo.setBasicDao(dao.getBasicDao());
        if (canvas.getRoyaltyToken() != null) {
            canvasForCreateWorkResVo.setRoyaltyToken(canvas.getRoyaltyToken());
        }
        String urlPrefix = String.format(ProtoDaoConstant.urlPrefix, ProtoDaoConstant.bucketName);
        String imageName = CodeUtil.generateCode('W');
        String workUri =
                urlPrefix + ProtoDaoConstant.metaBucketName + ProtoDaoConstant.workBucketName + "/" + imageName + ".json";
        canvasForCreateWorkResVo.setWorkUri(workUri);
        canvasForCreateWorkResVo.setWorkUriRandom(imageName);
        canvasForCreateWorkResVo.setCanId(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
        if (dao.getDaoVersion() >= 1 && dao.getFixedReserveRatio() != null) {
            DaoReserveRatio daoReserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
            if (daoReserveRatio.getDaoMintFee() != null) {
                canvasForCreateWorkResVo
                        .setFixedPricedaoMintFee(daoReserveRatio.getDaoMintFee().stripTrailingZeros().toPlainString());
                canvasForCreateWorkResVo.setFixedDaoReserveRatio(daoReserveRatio);
            }

        }
        if (dao.getDaoVersion() >= 1 && dao.getUnfixedReserveRatio() != null) {
            DaoReserveRatio daoReserveRatio =
                    JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
            if (daoReserveRatio.getDaoMintFee() != null) {
                canvasForCreateWorkResVo
                        .setDaoMintFee(daoReserveRatio.getDaoMintFee().stripTrailingZeros().toPlainString());
                canvasForCreateWorkResVo.setDaoReserveRatio(daoReserveRatio);
            }
        }

        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
            canvasForCreateWorkResVo.setErc20PaymentMode(true);
        }

        canvasForCreateWorkResVo.setDaoErc20Address(dao.getErc20Token());

        canvasForCreateWorkResVo.setPayCurrencyType(dao.getPayCurrencyType());
        canvasForCreateWorkResVo.setInputTokenLogo(dao.getInputTokenLogo());
        canvasForCreateWorkResVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        canvasForCreateWorkResVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        canvasForCreateWorkResVo.setDaoSymbol(dao.getDaoSymbol());
        return canvasForCreateWorkResVo;
    }
}
