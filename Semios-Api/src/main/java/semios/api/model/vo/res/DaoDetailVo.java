package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.entity.User;
import semios.api.model.enums.BasicDaoEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.ICanvasService;
import semios.api.service.IUserService;
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
public class DaoDetailVo implements Serializable {

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
     * dao的背景图片地址
     */
    private String daoBgBanner;

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
     * dao创建者名字
     */
    private String userName;

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

    /**
     * dao中canvas数量
     */
    private Integer canvasNumber = 0;

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
     * 距离开始还有多少秒
     */
    private Long startTime = -1L;

    /**
     * 停机提示信息
     */
    private String pausedMsg;

    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * erc721地址
     */
    private String erc721Address;

    /**
     * feel pool address
     */
    private String feelPool;

    /**
     * 社交链接
     *
     * @mock ["http://123.com","http://456.com"]
     */
    private List<String> socialLinks;

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
     * mainDao dao id
     */
    private Integer mainDaoId;

    /**
     * mainDao dao projectId
     */
    private String mainDaoProjectId;

    /**
     * mainDao dao名称
     */
    private String mainDaoName;

    /**
     * 是否为聚合dao，false-否，true-是
     */
    private Boolean isTogetherDao;

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

    public static DaoDetailVo transfer(Dao dao, DaoDrbStatistics daoDrbStatistics) {
        DaoDetailVo daoDetailVo = new DaoDetailVo();
        daoDetailVo.setDaoId(dao.getId());
        daoDetailVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoDetailVo.setDaoBgBanner(dao.getDaoBgBanner());
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
        daoDetailVo.setErc721Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc721Token()));
        daoDetailVo.setFeelPool(CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
        daoDetailVo.setBasicDao(dao.getBasicDao());
        daoDetailVo.setTopupMode(Integer.valueOf(1).equals(dao.getTopupMode()));
        daoDetailVo.setMainDaoId(dao.getId());
        daoDetailVo.setMainDaoName(dao.getDaoName());
        daoDetailVo.setMainDaoProjectId(dao.getProjectId());
        daoDetailVo.setIsTogetherDao(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getIsTogetherDao()));
        daoDetailVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoDetailVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoDetailVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoDetailVo.setInputTokenDecimals(dao.getInputTokenDecimals());

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


        if (StringUtils.isNotBlank(dao.getRoyaltyFee()) && !"null".equals(dao.getRoyaltyFee())) {
            log.info("getRoyaltyFee:{}", dao.getRoyaltyFee());
            daoDetailVo.setRoyaltyFee(new BigDecimal(dao.getRoyaltyFee()).divide(new BigDecimal(ProtoDaoConstant.RATIO_BASE)).toPlainString());
        }

        daoDetailVo.setWhiteList((dao.getCanvasCreatedWhitelist() + dao.getCanvasCreatedWhitelistNft() + dao.getMinterWorksWhitelist() + dao.getMinterWorksWhitelistNft()
                + dao.getCanvasCreatedBlacklist() + dao.getMinterWorksBlacklist() + dao.getGlobalMintCap() + dao.getMintCap() + dao.getErc721MintCap()) + dao.getErc721MintCapId() > 0);
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
        if (DaoStatusEnum.NOT_STARTED.getStatus().equals(dao.getDaoStatus())) {
            LocalDate startDate = dao.getDaoStartDate();
            long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            // long startDateTime =
            // startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
            long startDateTime = startDate.atStartOfDay().toInstant(ZoneOffset.of("+8")).getEpochSecond();
            daoDetailVo.setStartTime(startDateTime - localTime);
        } else {
            if (dao.getDaoStartDate() != null) {
                daoDetailVo.setStartDate(dao.getDaoStartDate().format(ProtoDaoConstant.FORMATTER));
            }
        }
        if (DaoStatusEnum.SHUT_DOWN.getStatus().equals(dao.getDaoStatus())) {
            String msg = "For security reasons, some functions under D4A@%s are temporarily suspended by the DAO.";
            msg = String.format(msg, dao.getDaoNumber());
            daoDetailVo.setPausedMsg(msg);
        }
        IUserService userService = SpringBeanUtil.getBean(IUserService.class);
        if (userService != null) {
            User user = userService.findUserByAddressHash(dao.getOwnerAddress());
            if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                daoDetailVo.setUserName(user.getUserName());
            }
        }
        daoDetailVo.setTotalNftCasting(dao.getTotalNftCasting());
        daoDetailVo.setFavoriteAmount(dao.getFavoriteAmount());

        if (daoDrbStatistics != null) {
            if (daoDrbStatistics.getDaoReward() != null) {
                daoDetailVo.setDaoToken(daoDrbStatistics.getDaoReward().longValue());
            }
            daoDetailVo.setCanvasNumber(daoDrbStatistics.getCanvas());
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

        // 每次单独查询canvas的数量
        Integer canvasAmount = 0;
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        if (canvasService != null) {
            canvasAmount = canvasService.listCanvasAmountByDaoId(dao.getId() + "");
            daoDetailVo.setCanvasNumber(canvasAmount);
        }
        if (DaoStatusEnum.FINISHED.getStatus().equals(dao.getDaoStatus())) {
            daoDetailVo.setWorkNumber(canvasAmount);
        } else {
            IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
            if (workService != null) {
                Integer workAmount = workService.selectWorkAmounts(dao.getId() + "");
                daoDetailVo.setWorkNumber(workAmount);
            }
        }

        return daoDetailVo;
    }

    public static void main(String[] args) {

        LocalDate startDate = LocalDate.of(2023, 4, 18);
        long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
        long startDateTime = startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
        System.out.println(startDateTime);
        System.out.println(localTime);
        System.out.println(startDateTime - localTime);
    }

    public void setDaoAssetPool(Float daoAssetPool) {
        if (daoAssetPool != null) {
            this.daoAssetPool = daoAssetPool;
        }
    }

    public void setCanvasNumber(Integer canvasNumber) {
        if (canvasNumber != null) {
            this.canvasNumber = canvasNumber;
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
