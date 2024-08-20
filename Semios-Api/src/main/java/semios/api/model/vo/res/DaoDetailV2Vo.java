package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.enums.BasicDaoEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.service.IWorkService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @description: DAO详情
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class DaoDetailV2Vo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * dao的logo地址
     */
    private String daoLogoUrl;

    /**
     * dao描述
     */
    private String daoDescription;

    /**
     * dao宣言
     */
    private String daoManitesto;

    /**
     * DAO编号
     */
    private Integer daoNumber;

    /**
     * dao当前状态 0-未开始 1-已开始 2-已结束
     */
    private Integer daoStatus;

    /**
     * dao创建者地址
     */
    private String creatorAddress;

    /**
     * feePool address
     */
    private String feePool;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * erc20地址,和上面的一致，vue3版本需要这个
     */
    private String daoErc20Address;

    /**
     * erc721地址
     */
    private String erc721Address;


    /**
     * opensea链接地址
     */
    private String openseaLink;

    /**
     * Twitter链接地址
     */
    private String twitterLink;

    /**
     * Discord链接地址
     */
    private String discordLink;

//    /**
//     * 社交链接
//     * <p>
//     * 社交链接 List<DaoSocialLinks> 的json对象
//     */
//    private List<DaoSocialLinksVo> socialLinks;

    /**
     * 社交链接
     *
     * @mock ["http://123.com","http://456.com"]
     */
    private List<String> socialLinks;


    /**
     * mainDaoId
     */
    private Integer mainDaoId;


    /**
     * dao总数
     */
    private Integer totalDaoNumber;


    /**
     * 倒计时还有多久到下一个mintWindow
     */
    private Long nextMintWindowStartTime = 0L;

    /**
     * 一个drb总的时间
     */
    private Long totalMintWindowTime = 1L;


    /**
     * Current mint window information
     */
    private MintWindowInfoVo mintWindowInfoVo;

    /**
     * Basic Information
     */
    private BasicInformationVo basicInformation;

    /**
     * Mode Status
     */
    private ModeStatusVo modeStatusVo;


    /**
     * dao中work数量
     */
    private Integer workNumber = 0;

    /**
     * dao中nft数量
     */
    private Integer nftNumber = 0;

    /**
     * 收藏数量
     */
    private Integer favoriteAmount = 0;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * DAO已经发放的代币数量
     */
    private Long daoToken = 0L;

    /**
     * DAO铸造的总收入
     */
    private Double mintRevenue = 0.0;

    /**
     * 可铸造NFT总量
     */
    private Integer totalNftCasting;

    /**
     * DRB的总量
     */
    private Integer daoMintWindow;


    /**
     * 停机提示信息
     */
    private String pausedMsg;


    /**
     * 是否可修改
     *
     * @mock false
     */
    private Boolean modifiable = false;

    /**
     * 二次售卖印花税
     *
     * @mock 0.075
     */
    private String royaltyFee;

    /**
     * 是否设置了黑白名单
     *
     * @mock false
     */
    private Boolean whiteList = false;

    /**
     * dao开始日期
     */
    private String startDate;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * dao当前已发生的drb数量
     */
    private Integer mintWindow;

    /**
     * DAO在该区块结束时资金池里的总金额
     */
    private Float daoAssetPool = 0.0f;

    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     *
     * @see BasicDaoEnum
     */
    private Integer basicDao;

    /**
     * daoFlow dao流水
     */
    private Float daoFlow;

    /**
     * 是否开启了TopUp模式 false-否 true-是
     */
    private Boolean topupMode;

    /**
     * 是否为MainDAO false-否 true-是
     */
    private Boolean isMainDaoCreator = false;

    /**
     * mainDao dao projectId
     */
    private String mainDaoProjectId;

    /**
     * mainDao dao名称
     */
    private String mainDaoName;

    /**
     * 距离开始还有多少秒
     */
    private Long startTime = -1L;

    /**
     * 百分比计算
     */
    private String proportion;

    /**
     * 倒计时时间
     */
    private Long countdown;

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

    /**
     * 是否为MainDAO 0-否 1-是
     * 1-SeedNodes
     * 0-SubNodes
     */
    private Integer isAncestorDao;

    public static DaoDetailV2Vo transfer(Dao dao, DaoDrbStatistics daoDrbStatistics) {
        DaoDetailV2Vo daoDetailVo = new DaoDetailV2Vo();
        daoDetailVo.setDaoId(dao.getId());
        daoDetailVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        if (StringUtils.isBlank(dao.getDaoDescription())) {
            daoDetailVo.setDaoDescription("");
        } else {
            daoDetailVo.setDaoDescription(dao.getDaoDescription());
        }
        if (StringUtils.isBlank(dao.getDaoManitesto())) {
            daoDetailVo.setDaoManitesto("");
        } else {
            daoDetailVo.setDaoManitesto(dao.getDaoManitesto());
        }
        daoDetailVo.setDaoName(dao.getDaoName());
        daoDetailVo.setDaoNumber(dao.getDaoNumber());
        daoDetailVo.setDaoStatus(dao.getDaoStatus());
        daoDetailVo.setDaoVersion(dao.getDaoVersion());
        daoDetailVo.setDaoLogoUrl(dao.getDaoLogoUrl());
        daoDetailVo.setDaoMintWindow(dao.getDaoMintWindow());
        daoDetailVo.setDiscordLink(dao.getDiscordLink());
        daoDetailVo.setOpenseaLink(dao.getOpenseaLink());
        daoDetailVo.setTwitterLink(dao.getTwitterLink());
        daoDetailVo.setCreatorAddress(dao.getOwnerAddress());
        daoDetailVo.setErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        daoDetailVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        daoDetailVo.setErc721Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc721Token()));
        daoDetailVo.setFeePool(CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
        daoDetailVo.setBasicDao(dao.getBasicDao());
        daoDetailVo.setTopupMode(Integer.valueOf(1).equals(dao.getTopupMode()));

        daoDetailVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoDetailVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoDetailVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoDetailVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoDetailVo.setDaoSymbol(dao.getDaoSymbol());
        daoDetailVo.setIsAncestorDao(dao.getIsAncestordao());

        if (DaoStatusEnum.NOT_STARTED.getStatus().equals(dao.getDaoStatus())) {
            LocalDate startDate = dao.getDaoStartDate();
            long localTime = LocalDateTime.now().toInstant(ZoneOffset.UTC).getEpochSecond();
            // long startDateTime =
            // startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
            long startDateTime = startDate.atStartOfDay().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            log.info("[DaoDetailV2Vo] daoId:{} localTime:{} startDateTime:{}", dao.getId(), localTime, startDateTime);
            daoDetailVo.setStartTime(startDateTime - localTime);
        } else {
            if (dao.getDaoStartDate() != null) {
                daoDetailVo.setStartDate(dao.getDaoStartDate().format(ProtoDaoConstant.FORMATTER));
            }
        }
//        daoDetailVo.setMainDaoId(dao.getId());
//        daoDetailVo.setMainDaoName(dao.getDaoName());
//        daoDetailVo.setMainDaoProjectId(dao.getProjectId());
        if (dao.getDaoFlow() != null) {
            daoDetailVo.setDaoFlow(dao.getDaoFlow().floatValue());
        }
//        ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
//        if (iSubscriptionService != null) {
//            Result<String> result =
//                    iSubscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
//            if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
//                log.info("[DaoDetailVo]infura ethGetBalance return data:{} daoId:{}", result.getData(),
//                        dao.getId());
//                String balance = result.getData();
//                String price = CommonUtil.hexToTenString(balance);
//                daoDetailVo.setDaoAssetPool(ProtoDaoCommonUtil.bigdecimalToFloat(
//                        new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR)));
//            } else {
//                log.error("[DaoDetailVo]infura ethGetBalance return data:{} daoId:{}", result.getData(),
//                        dao.getId());
//            }
//        }
        CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
        if (commonService != null) {
            daoDetailVo.setDaoAssetPool(ProtoDaoCommonUtil.bigdecimalToFloat(commonService.getInputToken(dao)));
        }


        daoDetailVo.setRoyaltyFee(
                new BigDecimal(dao.getRoyaltyFee()).divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)).toPlainString());
        daoDetailVo.setWhiteList((dao.getCanvasCreatedWhitelist() + dao.getMinterWorksWhitelist()
                + dao.getCanvasCreatedBlacklist() + dao.getMinterWorksBlacklist() + dao.getGlobalMintCap() + dao.getMintCap() + dao.getErc721MintCap()) > 0);
        if (StringUtils.isNotBlank(dao.getSocialLinks())) {
            daoDetailVo.setSocialLinks(new ArrayList<>(Arrays.asList(dao.getSocialLinks().split(","))));
        } else {
            daoDetailVo.setSocialLinks(Arrays.asList("", "", ""));
        }
        for (int i = 0; i < 3; i++) {
            if (daoDetailVo.getSocialLinks().size() < 3) {
                daoDetailVo.getSocialLinks().add("");
            }
        }
//        if (DaoStatusEnum.NOT_STARTED.getStatus().equals(dao.getDaoStatus())) {
//            LocalDate startDate = dao.getDaoStartDate();
//            long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
//            // long startDateTime =
//            // startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
//            long startDateTime = startDate.atStartOfDay().toInstant(ZoneOffset.of("+8")).getEpochSecond();
//        } else {
//            daoDetailVo.setStartDate(dao.getDaoStartDate().format(ProtoDaoConstant.FORMATTER));
//        }
        if (DaoStatusEnum.SHUT_DOWN.getStatus().equals(dao.getDaoStatus())) {
            String msg = "For security reasons, some functions under D4A@%s are temporarily suspended by the DAO.";
            msg = String.format(msg, dao.getDaoNumber());
            daoDetailVo.setPausedMsg(msg);
        }
        daoDetailVo.setTotalNftCasting(dao.getTotalNftCasting());
        daoDetailVo.setFavoriteAmount(dao.getFavoriteAmount());

        if (daoDrbStatistics != null) {
            if (daoDrbStatistics.getDaoReward() != null) {
                daoDetailVo.setDaoToken(daoDrbStatistics.getDaoReward().longValue());
            }
            if (daoDrbStatistics.getNft() != null) {
                daoDetailVo.setNftNumber(Integer.valueOf(daoDrbStatistics.getNft()));
            }
            if (daoDrbStatistics.getWorks() != null) {
                daoDetailVo.setWorkNumber(daoDrbStatistics.getWorks());
            }

            if (daoDrbStatistics.getMintRevenueExTax() != null) {
                daoDetailVo.setMintRevenue(daoDrbStatistics.getMintRevenueExTax().doubleValue());
            }
        }


        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        if (workService != null) {
            Integer workAmount = workService.selectNftAmounts(dao.getId() + "");
            daoDetailVo.setWorkNumber(workAmount);
        }


        return daoDetailVo;
    }

    public static void main(String[] args) {

        LocalDate startDate = LocalDate.of(2024, 1, 16);
        long localTime = LocalDateTime.now().toInstant(ZoneOffset.UTC).getEpochSecond();
        long startDateTime = startDate.atStartOfDay().toInstant(ZoneOffset.UTC).getEpochSecond();
        System.out.println(startDateTime);
        System.out.println(localTime);
        System.out.println(startDateTime - localTime);

//        LocalDate startDate = LocalDate.of(2024, 1, 16);
//        long localTime = LocalDateTime.now().toInstant(ZoneOffset.UTC).getEpochSecond();
//        // long startDateTime =
//        // startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
//        long startDateTime = startDate.atStartOfDay().toInstant(ZoneOffset.UTC).getEpochSecond();
//        System.out.println(startDateTime);
//        System.out.println(localTime);
//        System.out.println(startDateTime - localTime);
    }

    public void setDaoAssetPool(Float daoAssetPool) {
        if (daoAssetPool != null) {
            this.daoAssetPool = daoAssetPool;
        }
    }

    public void setWorkNumber(Integer workNumber) {
        if (workNumber != null) {
            this.workNumber = workNumber;
        }
    }

    public void setNftNumber(Integer nftNumber) {
        if (nftNumber != null) {
            this.nftNumber = nftNumber;
        }
    }

    public void setFavoriteAmount(Integer favoriteAmount) {
        if (favoriteAmount != null) {
            this.favoriteAmount = favoriteAmount;
        }
    }

    public void setDaoToken(Long daoToken) {
        if (daoToken != null) {
            this.daoToken = daoToken;
        }
    }

    public void setMintRevenue(Double mintRevenue) {
        if (mintRevenue != null) {
            this.mintRevenue = mintRevenue;
        }
    }

    public void setTotalNftCasting(Integer totalNftCasting) {
        if (totalNftCasting != null) {
            this.totalNftCasting = totalNftCasting;
        }
    }
}
