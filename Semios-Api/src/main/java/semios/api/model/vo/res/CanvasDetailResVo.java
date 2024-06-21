package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.*;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.service.ICanvasDrbStatisticsService;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.service.IUserService;
import semios.api.service.IWorkService;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.List;

/**
 * @description: canvas详情展示信息
 * @author: xiangbin
 * @create: 2022-08-05 14:52
 **/
@Slf4j
@Data
public class CanvasDetailResVo {

    /**
     * canvas id
     *
     * @mock 10
     */
    private Integer canvasId;
    /**
     * canvas名称
     *
     * @mock Test Name
     */
    private String canvasName;

    /**
     * canvas的logo地址
     *
     * @mock http://dao4art.log.123456.png
     */
    private String canvasLogo;

    /**
     * canvas所属dao编号
     *
     * @mock 10
     */
    private Integer daoNumber;

    /**
     * canvas编号
     *
     * @mock 10
     */
    private Integer canvasNumber;

    /**
     * canvas创建者地址
     *
     * @mock 0x56dBa60a326C8a1e1ED148486A2695884Aa34e3b
     */
    private String creatorAddress;

    /**
     * canvas创建者名字
     *
     * @mock doc name
     */
    private String userName;

    /**
     * opensea链接地址
     *
     * @mock http://test-opensea.io/12342432/345
     */
    private String openseaLink = "";

    /**
     * Twitter链接地址
     *
     * @mock http://test-opensea.io/12342432/345
     */
    private String twitterLink = "";

    /**
     * Discord链接地址
     *
     * @mock http://test-opensea.io/12342432/345
     */
    private String discordLink = "";

    /**
     * canvas描述
     *
     * @mock description for canvas
     */
    private String canvasDescription;

    /**
     * 收藏数量
     *
     * @mock 10
     */
    private Integer favoriteAmount = 0;

    /**
     * 是否被当前用户收藏
     *
     * @mock false
     */
    private Boolean favorited = false;

    /**
     * canvas下work数量
     *
     * @mock 10
     */
    private Integer workAmount = 0;

    /**
     * canvas下nft数量
     *
     * @mock 10
     */
    private Integer nftAmount = 0;

    /**
     * Canvas被发放过的ERC20的数量
     *
     * @mock 0L
     */
    private Long daoToken = 0L;

    /**
     * Canvas本区块加上六个区块的铸造总费用占DAO在本区块加上六个区块铸造总费用的占比
     */
    private Float sevenDayNtrv = 0.0f;

    /**
     * Canvas的铸造总收入
     */
    private Double mintRevenue = 0.0;

    /**
     * 2-已开始3-已结束 4-已停机
     */
    private Integer daoStatus;

    /**
     * 0-未创建1-已创建 2-已停机
     */
    private Integer canvasStatus;

    /**
     * dao name
     */
    private String daoName;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * 停机提示信息
     */
    private String pausedMsg;

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
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * canvas设置的royalty token 比例 默认为空
     */
    private BigDecimal royaltyToken;

    public static CanvasDetailResVo transfer(Canvas canvas, CanvasDrbStatistics canvasDrbStatistics, Dao dao) {
        CanvasDetailResVo canvasDetailResVo = new CanvasDetailResVo();
        canvasDetailResVo.setCanvasId(canvas.getId());
        canvasDetailResVo.setDaoId(canvas.getDaoId());
        canvasDetailResVo.setDaoVersion(dao.getDaoVersion());
        if (canvas.getRoyaltyToken() != null) {
            canvasDetailResVo.setRoyaltyToken(canvas.getRoyaltyToken());
        }
        canvasDetailResVo.setCanvasLogo(canvas.getCanvasLogo());
        canvasDetailResVo.setCanvasName(canvas.getCanvasName());
        canvasDetailResVo.setCanvasDescription(canvas.getCanvasDescription());
        canvasDetailResVo.setCanvasNumber(canvas.getCanvasNumber());
        canvasDetailResVo.setCreatorAddress(canvas.getOwnerAddress());
        IUserService userService = SpringBeanUtil.getBean(IUserService.class);
        if (userService != null) {
            User user = userService.findUserByAddressHash(canvas.getOwnerAddress());
            if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                canvasDetailResVo.setUserName(user.getUserName());
            }
        }
        canvasDetailResVo.setDaoNumber(canvas.getDaoNumber());
        canvasDetailResVo.setDiscordLink(canvas.getDiscordLink());
        canvasDetailResVo.setOpenseaLink(canvas.getOpenseaLink());
        canvasDetailResVo.setTwitterLink(canvas.getTwitterLink());
        canvasDetailResVo.setDaoStatus(canvas.getDaoStatus());
        canvasDetailResVo.setCanvasStatus(canvas.getCanvasStatus());
        canvasDetailResVo.setDaoName(dao.getDaoName());
        if (StringUtils.isNotBlank(canvas.getSocialLinks())) {
            canvasDetailResVo.setSocialLinks(Arrays.asList(canvas.getSocialLinks().split(",")));
        }
        canvasDetailResVo.setFavoriteAmount(canvas.getFavoriteAmount());

        if (canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
            String msg =
                    "For security reasons, some functions under Dao (D4A@%s) are temporarily suspended by the DAO.";
            msg = String.format(msg, canvas.getDaoNumber());
            canvasDetailResVo.setPausedMsg(msg);
        }

        if (canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
            String msg =
                    "For security reasons, some functions under Canvas (D4A@%s/Canvas*%s) are temporarily suspended by the Canvas.";
            msg = String.format(msg, canvas.getDaoNumber(), canvas.getCanvasNumber());
            canvasDetailResVo.setPausedMsg(msg);
        }

        if (canvasDrbStatistics != null) {
            if (canvasDrbStatistics.getDaoReward() != null) {
                canvasDetailResVo.setDaoToken(canvasDrbStatistics.getDaoReward().longValue());
            }
            if (canvasDrbStatistics.getMintRevenueExTax() != null) {
                canvasDetailResVo.setMintRevenue(canvasDrbStatistics.getMintRevenueExTax().doubleValue());
            }

            Integer drbNumber = Integer.valueOf(dao.getCurrentRound());
            if (canvasDrbStatistics.getDrbNumber() == null || drbNumber - canvasDrbStatistics.getDrbNumber() <= 6) {
                Integer startNumber = drbNumber > 6 ? drbNumber - 6 : 0;
                ICanvasDrbStatisticsService canvasDrbStatisticsService =
                        SpringBeanUtil.getBean(ICanvasDrbStatisticsService.class);
                IDaoDrbStatisticsService daoDrbStatisticsService =
                        SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
                IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
                if (canvasDrbStatisticsService != null) {
                    CanvasDrbStatistics canvasDrbStatistics1 = canvasDrbStatisticsService
                            .selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", startNumber, drbNumber);
                    if (daoDrbStatisticsService != null && workService != null) {
                        log.info("[CanvasDetailResVo]daoDrbStatisticsService is not null");
                        List<DaoDrbStatistics> daoDrbStatisticsList1 =
                                daoDrbStatisticsService.selectGalleryDao(startNumber, drbNumber);
                        Integer daoId = canvasDrbStatistics.getDaoId();
                        log.info("[CanvasDetailResVo]daoId:{}", daoId);
                        BigDecimal sevenDayDrbVol =
                                daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(daoId))
                                        .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                        log.info("[CanvasDetailResVo]canvasDrbStatistics1:{} sevenDayDrbVol:{}",
                                JacksonUtil.obj2json(canvasDrbStatistics1), sevenDayDrbVol.toPlainString());
                        if (canvasDrbStatistics1 != null && canvasDrbStatistics1.getSevenDayDrbVol() != null && canvasDrbStatistics1.getSevenDayDrbVol().compareTo(BigDecimal.ZERO) > 0
                                && sevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0) {
                            canvasDetailResVo.setSevenDayNtrv(
                                    ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics1.getSevenDayDrbVol()
                                            .divide(sevenDayDrbVol, 4, RoundingMode.FLOOR).multiply(new BigDecimal("100"))));
                        } else {
                            int canvasCount = workService.selectRangeNftCountByCanvasId(canvasDrbStatistics.getCanvasId() + "", startNumber, Integer.valueOf(dao.getCurrentRound()));
                            int daoCount = workService.selectRangeNftCountByDaoId(dao.getId() + "", startNumber, Integer.valueOf(dao.getCurrentRound()));
                            if (canvasCount > 0 && daoCount > 0) {
                                canvasDetailResVo.setSevenDayNtrv(ProtoDaoCommonUtil.bigdecimalToFloat(
                                        new BigDecimal(String.valueOf(canvasCount)).divide(new BigDecimal(String.valueOf(daoCount)), 4, RoundingMode.FLOOR).multiply(new BigDecimal("100"))));
                            }
                        }
                    }
                }
            }

            canvasDetailResVo.setNftAmount(canvasDrbStatistics.getNft());
            if (DaoStatusEnum.FINISHED.getStatus().equals(canvas.getDaoStatus())) {
                canvasDetailResVo.setWorkAmount(canvasDrbStatistics.getNft());
            }
        }

        if (!DaoStatusEnum.FINISHED.getStatus().equals(canvas.getDaoStatus())) {
            IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
            if (workService != null) {
                Integer workAmount = workService.selectWorkAmountsByCanvasId(canvas.getCanvasId());
                canvasDetailResVo.setWorkAmount(workAmount);
            }
        }

        return canvasDetailResVo;
    }

    public static void main(String[] args) {
        // CanvasDrbStatistics canvasDrbStatistics = new CanvasDrbStatistics();
        // canvasDrbStatistics.setMintRevenue(new BigDecimal("0.045"));
        // canvasDrbStatistics.getMintRevenue()
        // .multiply(BigDecimal.ONE.subtract(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))
        // .add(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))))).doubleValue();
        // System.out.println(BigDecimal.ONE.subtract(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))
        // .add(new BigDecimal(Dao4ArtConstant.MINT_D4A_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE)))));
        // System.out.println(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))
        // .add(new BigDecimal(Dao4ArtConstant.MINT_D4A_FEE_RATIO).divide(new BigDecimal(Dao4ArtConstant.RATIO_BASE))));
        // System.out.println(canvasDrbStatistics.getMintRevenue()
        // .multiply(BigDecimal.ONE.subtract(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))
        // .add(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))))).doubleValue());
    }

    public void setOpenseaLink(String openseaLink) {
        if (StringUtils.isNotBlank(openseaLink) && !"undefined".equals(openseaLink)) {
            this.openseaLink = openseaLink;
        }
    }

    public void setTwitterLink(String twitterLink) {
        if (StringUtils.isNotBlank(twitterLink) && !"undefined".equals(twitterLink)) {
            this.twitterLink = twitterLink;
        }
    }

    public void setDiscordLink(String discordLink) {
        if (StringUtils.isNotBlank(discordLink) && !"undefined".equals(discordLink)) {
            this.discordLink = discordLink;
        }
    }

    public void setFavoriteAmount(Integer favoriteAmount) {
        if (favoriteAmount != null) {
            this.favoriteAmount = favoriteAmount;
        }
    }

    public void setWorkAmount(Integer workAmount) {
        if (workAmount != null) {
            this.workAmount = workAmount;
        }
    }

    public void setNftAmount(Integer nftAmount) {
        if (nftAmount != null) {
            this.nftAmount = nftAmount;
        }
    }

    public void setDaoToken(Long daoToken) {
        if (daoToken != null) {
            this.daoToken = daoToken;
        }
    }

    public void setSevenDayNtrv(Float sevenDayNtrv) {
        if (sevenDayNtrv != null) {
            this.sevenDayNtrv = sevenDayNtrv;
        }
    }

    public void setMintRevenue(Double mintRevenue) {
        if (mintRevenue != null) {
            this.mintRevenue = mintRevenue;
        }
    }
}
