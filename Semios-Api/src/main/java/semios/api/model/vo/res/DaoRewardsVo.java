package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.service.ICanvasDrbStatisticsService;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.service.IWorkService;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * @description: dao下面每个canvasr的rewards
 * @author: xiangbin
 * @create: 2022-08-04 18:47
 **/
@Data
public class DaoRewardsVo {

    /**
     * canvas id
     */
    private Integer canvasId;

    /**
     * canvas logo
     */
    private String canvasLogo;

    /**
     * canvas名称
     */
    private String canvasName;

    /**
     * canvas当前铸造价格
     */
    private Float mintPrice;

    /**
     * canvas当前区块的铸造费用
     */
    private Float rdbVol = 0.0f;

    /**
     * canvas当前区块的值加上六个区块（包括轮空的区块）的铸造费用
     */
    private Float sevenDayRdbVol = 0.0f;

    /**
     * canvas的总铸造费用
     */
    private Float totalVol = 0.0f;

    /**
     * canvas的7D DRB Vol占DAO的7D DRB Vol的比例
     */
    private Float sevenDayNtvr = 0.0f;

    /**
     * canvas的总铸造收益占DAO总铸造收益的比例
     */
    private Float ntvr = 0.0f;

    /**
     * Canvas 中已铸造的艺术作品所在地址的私钥拥有者去重计数
     */
    private Integer owners = 0;

    /**
     * Canvas 中已铸造了多少个NFT
     */
    private Integer nfts = 0;
    /**
     * dao 状态 0-未创建1-已创建未开始2-已开始3-已结束
     */
    private Integer daoStatus;

    public static DaoRewardsVo transfer(CanvasDrbStatistics canvasDrbStatistics, Dao dao) {
        DaoRewardsVo daoRewardsVo = new DaoRewardsVo();
        daoRewardsVo.setCanvasId(canvasDrbStatistics.getCanId());
        daoRewardsVo.setCanvasName(canvasDrbStatistics.getCanvasName());
        daoRewardsVo.setCanvasLogo(canvasDrbStatistics.getCanvasLogo());

        if (canvasDrbStatistics.getCurrentPrice() != null) {
            daoRewardsVo.setMintPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getCurrentPrice()));
        } else {
            daoRewardsVo.setMintPrice(ProtoDaoCommonUtil.bigdecimalToFloat(dao.getDaoFloorPrice()));
        }

        daoRewardsVo.setNfts(canvasDrbStatistics.getNft());

        ICanvasDrbStatisticsService canvasDrbStatisticsService = SpringBeanUtil.getBean(ICanvasDrbStatisticsService.class);
        IDaoDrbStatisticsService daoDrbStatisticsService = SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        if (canvasDrbStatisticsService != null) {
            Integer startDrb = Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) >= 6 ? Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND) - 6 : 0;
            BigDecimal canvasSevenDayDrbVol;
            CanvasDrbStatistics canvasDrbStatistics1 = canvasDrbStatisticsService.selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", startDrb, Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
            if (canvasDrbStatistics1 != null && canvasDrbStatistics1.getSevenDayDrbVol() != null) {
                canvasSevenDayDrbVol = canvasDrbStatistics1.getSevenDayDrbVol();
                daoRewardsVo.setSevenDayRdbVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasSevenDayDrbVol));
                if (daoDrbStatisticsService != null && workService != null) {
                    List<DaoDrbStatistics> daoDrbStatisticsList1 = daoDrbStatisticsService.selectGalleryDao(startDrb, Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                    Integer daoId = canvasDrbStatistics.getDaoId();
                    BigDecimal sevenDayDrbVol = daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(daoId)).map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                    if (canvasSevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0 && sevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0) {
                        daoRewardsVo.setSevenDayNtvr(ProtoDaoCommonUtil.bigdecimalToFloat(canvasSevenDayDrbVol.divide(sevenDayDrbVol, 4, RoundingMode.FLOOR).multiply(new BigDecimal("100"))));
                    } else {
                        int canvasCount = workService.selectRangeNftCountByCanvasId(canvasDrbStatistics.getCanvasId() + "", startDrb, Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                        int daoCount = workService.selectRangeNftCountByDaoId(dao.getId() + "", startDrb, Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                        if (canvasCount > 0 && daoCount > 0) {
                            daoRewardsVo.setSevenDayNtvr(ProtoDaoCommonUtil.bigdecimalToFloat(
                                    new BigDecimal(String.valueOf(canvasCount)).divide(new BigDecimal(String.valueOf(daoCount)), 4, RoundingMode.FLOOR).multiply(new BigDecimal("100"))));
                        }
                    }
                }
            }
        }

        if (Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).equals(canvasDrbStatistics.getDrbNumber())) {
            //计算每个canvas的dre 其他区块为零
            if (workService != null && daoDrbStatisticsService != null) {
                Integer drbNumber = Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND);
                Double drbVol = workService.selectNftMintedPriceByDrbAndCanId(canvasDrbStatistics.getCanvasId() + "", drbNumber + "");
                if (drbVol == null) {
                    drbVol = 0.0;
                }
                DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(canvasDrbStatistics.getDaoId(), drbNumber);
                if (daoDrbStatistics != null && daoDrbStatistics.getDrbVol() != null && drbVol.compareTo(0.0) > 0 && daoDrbStatistics.getDrbVol().compareTo(BigDecimal.ZERO) > 0) {
                    daoRewardsVo.setNtvr(new BigDecimal(String.valueOf(drbVol)).divide(daoDrbStatistics.getDrbVol(), 6, RoundingMode.FLOOR).floatValue());
                } else {
                    int canvasCount = workService.selectDrbNftCountByCanvasId(canvasDrbStatistics.getCanvasId() + "", Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                    int daoCount = workService.selectDrbNftCountByDaoId(dao.getId() + "", Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND));
                    if (canvasCount > 0 && daoCount > 0) {
                        daoRewardsVo.setNtvr(ProtoDaoCommonUtil.bigdecimalToFloat(
                                new BigDecimal(String.valueOf(canvasCount)).divide(new BigDecimal(String.valueOf(daoCount)), 6, RoundingMode.FLOOR)));
                    }
                }
            }
        }
//        if(daoDrbStatisticsService != null) {
//            DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(canvasDrbStatistics.getDaoId());
//            if (daoDrbStatistics != null && daoDrbStatistics.getMintRevenue() != null) {
//                daoRewardsVo.setNtvr(canvasDrbStatistics.getTotalVol().divide(daoDrbStatistics.getMintRevenue(), 6, RoundingMode.FLOOR).floatValue());
//            }
//        }

        daoRewardsVo.setOwners(canvasDrbStatistics.getOwners());
        if (Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).equals(canvasDrbStatistics.getDrbNumber())) {
            daoRewardsVo.setRdbVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getDrbVol()));
        }

        daoRewardsVo.setTotalVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getTotalVol()));

        daoRewardsVo.setDaoStatus(dao.getDaoStatus());

        return daoRewardsVo;
    }

    public void setOwners(Integer owners) {
        if (owners != null) {
            this.owners = owners;
        }
    }

    public void setNfts(Integer nfts) {
        if (nfts != null) {
            this.nfts = nfts;
        }
    }

    public void setNtvr(Float ntvr) {
        if (ntvr != null) {
            this.ntvr = new BigDecimal(ntvr).multiply(new BigDecimal("100")).setScale(4, RoundingMode.FLOOR).stripTrailingZeros().floatValue();
        }
    }

    public void setRdbVol(Float rdbVol) {
        if (rdbVol != null) {
            this.rdbVol = rdbVol;
        }
    }

    public void setSevenDayRdbVol(Float sevenDayRdbVol) {
        if (sevenDayRdbVol != null) {
            this.sevenDayRdbVol = sevenDayRdbVol;
        }
    }

    public void setTotalVol(Float totalVol) {
        if (totalVol != null) {
            this.totalVol = totalVol;
        }
    }

    public void setSevenDayNtvr(Float sevenDayNtvr) {
        if (sevenDayNtvr != null) {
            this.sevenDayNtvr = sevenDayNtvr;
        }
    }
}
