package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.enums.WorkPriceTypeEnum;
import semios.api.model.vo.res.BaseWorkVo.BaseWorkListVo;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.DateUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;

/**
 * @description: DAO下未铸造的Work列表
 * @author: xiangbin
 * @create: 2022-08-04 18:08
 **/
@Slf4j
@Data
public class WorkListVo {


    private Integer daoId;
    /**
     * work id
     */
    private Integer workId;

    /**
     * work图片地址
     */
    private String imgUrl;

    /**
     * work描述信息
     */
    private String workDescription;

    /**
     * work所属的dao编号
     */
    private Integer daoNumber;
    /**
     * work所属的canvas编号
     */
    private Integer canvasNumber;

    /**
     * work铸造出来的编号即nft编号，未铸造时返回空
     */
    private Integer workNumber;

    /**
     * work铸造价格
     */
    private String price = "0";

    /**
     * work的收藏次数
     */
    private Integer favoriteAmount;

    /**
     * 背景颜色 宽260，然后缩放后高度，然后加上背景色
     */
    private String bgColor;

    /**
     * nft高度 宽260之后的高度
     */
    private Double height;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * 是否为一口价 version_1.5
     *
     * @mock false
     */
    private Boolean fixedPrice = false;

    /**
     * canvas设置royaltyToken后的价格 1.9
     */
    private BigDecimal royaltyTokenPrice;

    /**
     * 是否自动生成 1-自动生成 2-上传的
     */
    private Integer generate;

    /**
     * work所属dao名称
     */
    private String daoName;
    /**
     * work创建者地址
     */
    private String creatorAddress;

    /**
     * Work状态0-已创建1-已铸造2-已失效
     */
    private Integer workStatus;

    /**
     * 是否为全局一口价
     */
    private Boolean unifiedPriceSet = false;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    /**
     * 1.4.3 铸造人数
     */
    private Integer minters = 0;

    /**
     * 1.4.3 铸造金额
     */
    private String mintFee = "0";

    /**
     * 1.4.3 铸造work的数量
     */
    private Integer mintedWorks = 0;

    /**
     * 1.4.3 该mint Window出块的资产 Token
     */
    private String blockRewardToken = "0";

    /**
     * 1.4.3 该mint Window出块的资产 eth
     */
    private String blockRewardEth = "0";

    /**
     * 1.4.3 这个DRB铸造会用于内部奖励的资产 Token
     */
    private String internalRewardToken = "0";

    /**
     * 1.4.3 这个DRB铸造会用于内部奖励的资产 eth
     */
    private String internalRewardEth = "0";

    //如果已经铸造了不展示
    /**
     * 1.4.3 该用户铸造这个作品会获得的奖励 Token -如果已经铸造成NFT则不需要展示
     */
    private String mintersMaxRewardToken = "0";

    /**
     * 1.4.3 该用户铸造这个作品会获得的奖励 eth-如果已经铸造成NFT则不需要展示
     */
    private String mintersMaxRewardEth = "0";

    /**
     * 1.4.3 是否开启了TopUp模式 false-否 true-是
     */
    private Boolean topupMode = false;

    /**
     * 1.4.3 设置的自动生成的数量
     */
    private Integer passesTotalQuantity;

    /**
     * 1.4.3 已经自动生成的被铸造的数量
     */
    private Integer havePassesQuantity;


    /**
     * 1.6.1 价格类型(0-Price)(1-Fixed Price)(2-Unified Price)
     * 0-浮动价格
     * 1-work的一口价
     * 2-dao的全局一口价
     */
    private Integer priceType;


    /**
     * 1.7 work所属的支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 work所属的input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 work所属的input token的decimals
     */
    private Integer inputTokenDecimals;


    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * erc20 address
     */
    private String daoErc20Address;


    public static WorkListVo transfor(Work work, Canvas canvas) {
        log.info("开始转换workId={},开始时间为:{}", work.getId(), DateUtil.getCurrentTimestamp());
        WorkListVo workListVo = new WorkListVo();
        workListVo.setDaoId(work.getDaoId());
        workListVo.setBgColor(work.getBgColor());
        workListVo.setCanvasNumber(work.getCanvasNumber());
        workListVo.setDaoNumber(work.getDaoNumber());
        workListVo.setWorkDescription(work.getWorkDescription());
        workListVo.setHeight(work.getHeight());
        // ImageUtil.imageUrlResized
        workListVo.setImgUrl(work.getImageUrl());
        workListVo.setFavoriteAmount(work.getFavoriteAmount());
        workListVo.setFavorited(work.getFavorited());
        workListVo.setGenerate(work.getGenerate());
        workListVo.setCreatorAddress(work.getCreatorAddress());
        workListVo.setFixedPrice(work.getFixedPrice() != null);
        workListVo.setWorkStatus(work.getWorkStatus());
        boolean fixPrice = false;
        BigDecimal price = null;

        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        Dao dao = daoService.getById(work.getDaoId());

        workListVo.setDaoName(dao.getDaoName());
        workListVo.setPayCurrencyType(dao.getPayCurrencyType());
        workListVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        workListVo.setInputTokenLogo(dao.getInputTokenLogo());
        workListVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        workListVo.setDaoSymbol(dao.getDaoSymbol());
        workListVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        // 未铸造时展示canvas的价格
        log.info("设置价格字段workId={},开始时间为:{}", work.getId(), DateUtil.getCurrentTimestamp());
        if (work.getMintedPrice() != null) {
            workListVo.setPrice(work.getMintedPrice().stripTrailingZeros().toPlainString());
            workListVo.setWorkNumber(work.getWorkNumber());
        } else {
            if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                workListVo.setPrice(dao.getGlobalDaoPrice().stripTrailingZeros().toPlainString());
            } else {
                if (canvas == null) {
                    // log.info("[WorkListVo-transfor] canvas is null");
                    ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
                    if (canvasService != null) {
                        log.info("[WorkListVo-transfor] canvasService not null");
                        if (work.getCanId() != null) {
                            canvas = canvasService.getById(work.getCanId());
                        }
                        if (canvas == null && StringUtils.isNotBlank(work.getCanvasId())) {
                            canvas = canvasService.selectCanvasDetailByCanvasId(CommonUtil.removeHexPrefixIfExists(work.getCanvasId()));
                        }
                        if (canvas == null) {
                            canvas = new Canvas();
                            CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
                            if (commonService != null && StringUtils.isNotBlank(work.getCanvasId())) {
                                canvas.setCurrentPrice(commonService.getCanvasNextPrice(dao.getProjectId(), work.getCanvasId(), commonService.getErc20DecimalIfErc20PayModel(dao), dao.getInputTokenDecimals()));
                            } else {
                                canvas.setCurrentPrice(dao.getCanvasFloorPrice());
                            }
                        }
                    }
                }
                if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
                    workListVo.setPrice(work.getFixedPrice().stripTrailingZeros().toPlainString());
                    price = work.getFixedPrice();
                    fixPrice = true;
                } else {
                    if (canvas != null) {
                        log.info("[WorkListVo-transfor] canvasId:{}", canvas.getId());
                        workListVo.setPrice(canvas.getCurrentPrice().stripTrailingZeros().toPlainString());
                        price = canvas.getCurrentPrice();
                    }
                }
                //1.9只有未铸造的显示划线价
                if (canvas != null && canvas.getRoyaltyToken() != null) {
                    DaoRoyaltyToken daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
                    if (daoRoyaltyToken.getMinterReward().compareTo(BigDecimal.ZERO) != 0) {
                        String reserveRatio = fixPrice ? dao.getFixedReserveRatio() : dao.getUnfixedReserveRatio();
                        DaoReserveRatio daoReserveRatio = JacksonUtil.json2pojo(reserveRatio, DaoReserveRatio.class);
                        BigDecimal canvasMintFee = daoReserveRatio.getCanvasMintFee().divide(new BigDecimal("100"));
                        workListVo.setRoyaltyTokenPrice(price.subtract(price.multiply(canvasMintFee).multiply(canvas.getRoyaltyToken().divide(new BigDecimal("100")))));
                        if (price.compareTo(workListVo.getRoyaltyTokenPrice()) == 0) {
                            workListVo.setRoyaltyTokenPrice(null);
                        }
                    }
                }
            }
        }
        log.info("设置价格字段workId={},结束时间为:{}", work.getId(), DateUtil.getCurrentTimestamp());

        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            workListVo.setUnifiedPriceSet(true);
        }

        //  1.6.1修改价格类型
        if (workListVo.getUnifiedPriceSet()) {
            workListVo.setPriceType(WorkPriceTypeEnum.DAO_GLOBAL_PRICE.getType());  // 全局一口价类型
        } else if (workListVo.getFixedPrice()) {
            workListVo.setPriceType(WorkPriceTypeEnum.FIXED_PRICE.getType());  // Work一口价类型
        } else {
            workListVo.setPriceType(WorkPriceTypeEnum.CANVAS_PRICE.getType());  // 浮动价格类型
        }

        workListVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));

        workListVo.setWorkId(work.getId());

        // 1.4.3 添加新建的base work list
        log.info("添加新建的base work list--workId={},开始时间为:{}", work.getId(), DateUtil.getCurrentTimestamp());
        BaseWorkListVo baseWorkListVo = BaseWorkListVo.transfer(work, dao);
        workListVo.setMinters(baseWorkListVo.getMinters());
        workListVo.setMintFee(baseWorkListVo.getMintFee());
        workListVo.setMintedWorks(baseWorkListVo.getMintedWorks());
        workListVo.setBlockRewardToken(baseWorkListVo.getBlockRewardToken());
        workListVo.setBlockRewardEth(baseWorkListVo.getBlockRewardEth());
        workListVo.setInternalRewardToken(baseWorkListVo.getInternalRewardToken());
        workListVo.setInternalRewardEth(baseWorkListVo.getInternalRewardEth());
        workListVo.setMintersMaxRewardToken(baseWorkListVo.getMintersMaxRewardToken());
        workListVo.setMintersMaxRewardEth(baseWorkListVo.getMintersMaxRewardEth());
        workListVo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));
        log.info("添加新建的base work list--workId={},结束时间为:{}", work.getId(), DateUtil.getCurrentTimestamp());

        if (work.getGenerate() == 1 && dao.getNeedMintableWork() != null && dao.getNeedMintableWork() == 0 && dao.getGenerateWorkSet() != null) {
            workListVo.setPassesTotalQuantity(dao.getGenerateWorkSet());
            IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
            int workCount = workService.selectCountNftGenerateWork(dao.getId());
            workListVo.setHavePassesQuantity(workCount);
        }

        return workListVo;
    }
}
