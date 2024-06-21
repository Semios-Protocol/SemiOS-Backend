package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.service.IDaoService;
import semios.api.service.common.CommonService;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * @description: dao ranking
 * @author: xiangbin
 * @create: 2022-08-04 18:47
 **/
@Slf4j
@Data
public class DaoRankingVo {

    /**
     * dao id
     */
    private String daoId;

    /**
     * dao logo
     */
    private String daoLogoUrl;
    /**
     * dao 名称
     */
    private String daoName;

    /**
     * project对应的asset pool 地址
     */
    private String feePool;

    /**
     * DAO在该区块结束时所有canvas的最低价
     */
    private Float floorPrice = 0.0f;

    /**
     * DRB Vol
     */
    private Float drbVol = 0.0f;

    /**
     * DAO在该区块和之前的六个区块（包括轮空的区块）铸造费用总和
     */
    private Float sevenDayRdbVol = 0.0f;

    /**
     * DAO在该区块结束时资金池里的总金额
     */
    private Float daoAssetPool = 0.0f;

    /**
     * DAO在该区块结束时canvas的数量
     */
    private Integer canvasNumber = 0;

    /**
     * DAO在该区块结束时所有NFT所在地址的私钥拥有者去重计数
     */
    private Integer ownersNumber = 0;

    /**
     * DAO在该区块结束时所有NFT的数量
     */
    private Integer nftNumber = 0;

    /**
     * dao状态 0-未创建1-已创建未开始2-已开始3-已结束
     */
    private Integer daoStatus;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    public static DaoRankingVo transfer(DaoDrbStatistics daoDrbStatistics) {
        DaoRankingVo daoRankingVo = new DaoRankingVo();
        if (daoDrbStatistics.getDaoId() == null) {
            daoRankingVo.setDaoId(daoDrbStatistics.getDaoItemId() + "");
        } else {
            daoRankingVo.setDaoId(daoDrbStatistics.getDaoId() + "");
        }
        // if(daoDrbStatistics.getDaoAssetPool() != null &&
        // Integer.valueOf(Dao4ArtConstant.CURRENT_ROUND).equals(daoDrbStatistics.getDrbNumber())){
        // daoRankingVo.setDaoAssetPool(daoDrbStatistics.getDaoAssetPool().setScale(4,
        // RoundingMode.FLOOR).floatValue());
        // }

        Dao dao = null;
        try {
            // ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
            IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
//            if (iSubscriptionService != null && daoService != null) {
//                dao = daoService.getById(daoDrbStatistics.getDaoId());
//                if (dao != null) {
//                    Result<String> result =
//                            iSubscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
//                    if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
//                        log.info("[DaoRankingVo]infura ethGetBalance return data:{} daoId:{}", result.getData(),
//                                dao.getId());
//                        String balance = result.getData();
//                        String price = CommonUtil.hexToTenString(balance);
//                        daoRankingVo.setDaoAssetPool(ProtoDaoCommonUtil.bigdecimalToFloat(
//                                new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR)));
//                    } else {
//                        log.error("[DaoRankingVo]infura ethGetBalance return data:{} daoId:{}", result.getData(),
//                                daoDrbStatistics.getDaoId());
//                    }
//                }
//            }
            CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
            if (commonService != null && daoService != null) {
                dao = daoService.getById(daoDrbStatistics.getDaoId());
                if (dao != null) {
                    daoRankingVo.setDaoAssetPool(ProtoDaoCommonUtil.bigdecimalToFloat(commonService.getInputToken(dao)));
                }
            }
        } catch (Exception e) {
            log.error("[DaoRankingVo]infura ethGetBalance daoId:{} e:{}", daoDrbStatistics.getDaoId(), e);
        }

        daoRankingVo.setDaoName(daoDrbStatistics.getDaoName());
        daoRankingVo.setDaoLogoUrl(daoDrbStatistics.getDaoLogourl());
        daoRankingVo.setFeePool(daoDrbStatistics.getFeePool());

        // 单独查询canvas的数量
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        Integer canvasAmount;
        daoRankingVo.setFloorPrice(ProtoDaoCommonUtil.bigdecimalToFloat(daoDrbStatistics.getCanvasFloorPrice()));
        if (canvasService != null) {
            canvasAmount = canvasService.listCanvasAmountByDaoId(daoDrbStatistics.getDaoId() + "");
            daoRankingVo.setCanvasNumber(canvasAmount);

            Canvas canvas = canvasService.listCanvasFloorPriceByDaoId(daoDrbStatistics.getDaoId() + "");
            if (canvas != null && canvas.getCurrentPrice() != null) {
                daoRankingVo.setFloorPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvas.getCurrentPrice()));
            }
        }

        if (dao != null && dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            daoRankingVo.setFloorPrice(ProtoDaoCommonUtil.bigdecimalToFloat(dao.getGlobalDaoPrice()));
        }

        daoRankingVo.setOwnersNumber(ProtoDaoCommonUtil.stringToInteger(daoDrbStatistics.getOwners()));
        daoRankingVo.setNftNumber(ProtoDaoCommonUtil.stringToInteger(daoDrbStatistics.getNft()));
        if (Integer.valueOf(dao.getCurrentRound()).equals(daoDrbStatistics.getDrbNumber())) {
            daoRankingVo.setDrbVol(ProtoDaoCommonUtil.bigdecimalToFloat(daoDrbStatistics.getDrbVol()));
            daoRankingVo.setSevenDayRdbVol(ProtoDaoCommonUtil.bigdecimalToFloat(daoDrbStatistics.getSevenDayDrbVol()));
        } else {
            int drbNumber = Integer.parseInt(dao.getCurrentRound());
            Integer startNumber = drbNumber > 6 ? drbNumber - 6 : 0;
            if (daoDrbStatistics.getDrbNumber() == null || drbNumber - daoDrbStatistics.getDrbNumber() <= 6) {
                // 查询sevenDayDrbVol
                IDaoDrbStatisticsService daoDrbStatisticsService =
                        SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
                if (daoDrbStatisticsService != null) {

                    List<DaoDrbStatistics> daoDrbStatisticsList1 =
                            daoDrbStatisticsService.selectGalleryDao(startNumber, drbNumber);
                    BigDecimal sevenDayDrbVol =
                            daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(daoDrbStatistics.getDaoId()))
                                    .map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                    daoRankingVo.setSevenDayRdbVol(ProtoDaoCommonUtil.bigdecimalToFloat(sevenDayDrbVol));
                }
            }
        }
        daoRankingVo.setDaoStatus(daoDrbStatistics.getDaoStatus());
        daoRankingVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(daoDrbStatistics.getErc20Pay()));

        return daoRankingVo;
    }

    public void setFloorPrice(Float floorPrice) {
        if (floorPrice != null) {
            this.floorPrice = floorPrice;
        }
    }

    public void setDrbVol(Float drbVol) {
        if (drbVol != null) {
            this.drbVol = drbVol;
        }
    }

    public void setSevenDayRdbVol(Float sevenDayRdbVol) {
        if (sevenDayRdbVol != null) {
            this.sevenDayRdbVol = sevenDayRdbVol;
        }
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

    public void setOwnersNumber(Integer ownersNumber) {
        if (ownersNumber != null) {
            this.ownersNumber = ownersNumber;
        }
    }

    public void setNftNumber(Integer nftNumber) {
        if (nftNumber != null) {
            this.nftNumber = nftNumber;
        }
    }

}
