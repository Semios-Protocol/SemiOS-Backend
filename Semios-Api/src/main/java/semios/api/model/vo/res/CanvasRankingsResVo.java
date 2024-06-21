package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.service.ICanvasDrbStatisticsService;
import semios.api.service.IDaoDrbStatisticsService;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * @description: rankings列表
 * @author: xiangbin
 * @create: 2022-08-05 15:29
 **/
@Slf4j
@Data
public class CanvasRankingsResVo {

    /**
     * Canvas id
     */
    private String canvasId;

    /**
     * Canvas名称
     */
    private String canvasName;

    /**
     * Canvas图片
     */
    private String canvasImage;

    /**
     * Canvas在该区块结束时的价格
     */
    private Float mintPrice = 0.0f;

    /**
     * Canvas在当前区块铸造费用总和
     */
    private Float drbVol = 0.0f;

    /**
     * 7D DRB Vol
     */
    private Float sevenDayDrbVol = 0.0f;

    /**
     * Canvas在本区块和之前所有区块铸造总费用
     */
    private Float totalVol = 0.0f;

    /**
     * 7D NTVR
     */
    private Float seventDayNtvr = 0.0f;


    /**
     * Canvas在该区块结束时所有NFT所在地址的私钥拥有者去重计数
     */
    private Integer owners = 0;

    /**
     * Canvas在该区块结束时所有NFT的数量
     */
    private Integer nfts = 0;

    /**
     * 2-已开始3-已结束
     */
    private Integer daoStatus;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;


    public static CanvasRankingsResVo transfer(CanvasDrbStatistics canvasDrbStatistics) {
        CanvasRankingsResVo canvasRankingsResVo = new CanvasRankingsResVo();
        canvasRankingsResVo.setCanvasId(canvasDrbStatistics.getCanId() + "");
        canvasRankingsResVo.setCanvasName(canvasDrbStatistics.getCanvasName());
        canvasRankingsResVo.setCanvasImage(canvasDrbStatistics.getCanvasLogo());
        canvasRankingsResVo.setFavorited(canvasDrbStatistics.getFavorited());
        if (Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).equals(canvasDrbStatistics.getDrbNumber())) {
            if (canvasDrbStatistics.getMintPrice() == null) {
                canvasRankingsResVo.setMintPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getCurrentPrice()));
            } else {
                canvasRankingsResVo.setMintPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getMintPrice()));
            }
        } else {
            canvasRankingsResVo.setMintPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getCurrentPrice()));
        }

        if (Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).equals(canvasDrbStatistics.getDrbNumber())) {
            canvasRankingsResVo.setDrbVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getDrbVol()));
            canvasRankingsResVo.setSevenDayDrbVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getSevenDayDrbVol()));
        }

        int drbNumber = Integer.parseInt(ProtoDaoConstant.CURRENT_ROUND);
        Integer startNumber = drbNumber > 6 ? drbNumber - 6 : 0;
        ICanvasDrbStatisticsService canvasDrbStatisticsService = SpringBeanUtil.getBean(ICanvasDrbStatisticsService.class);
        IDaoDrbStatisticsService daoDrbStatisticsService = SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
        if (canvasDrbStatisticsService != null) {
            CanvasDrbStatistics canvasDrbStatistics1 = canvasDrbStatisticsService.selectByRangeDrb(canvasDrbStatistics.getCanvasId() + "", startNumber, drbNumber);
            if (canvasDrbStatistics1 != null) {
                canvasRankingsResVo.setSevenDayDrbVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics1.getSevenDayDrbVol()));
            }
            if (daoDrbStatisticsService != null) {
                log.info("[CanvasRankingsResVo]daoDrbStatisticsService is not null");
                List<DaoDrbStatistics> daoDrbStatisticsList1 = daoDrbStatisticsService.selectGalleryDao(startNumber, drbNumber);
                Integer daoId = canvasDrbStatistics.getDaoId();
                log.info("[CanvasRankingsResVo]daoId:{}", daoId);
                BigDecimal sevenDayDrbVol = daoDrbStatisticsList1.stream().filter(v -> v.getDaoId().equals(daoId)).map(DaoDrbStatistics::getDrbVol).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                log.info("[CanvasRankingsResVo]canvasDrbStatistics1:{} sevenDayDrbVol:{}", JacksonUtil.obj2json(canvasDrbStatistics1), sevenDayDrbVol.toPlainString());
                if (canvasDrbStatistics1 != null && canvasDrbStatistics1.getSevenDayDrbVol() != null && sevenDayDrbVol.compareTo(BigDecimal.ZERO) > 0) {
                    canvasRankingsResVo.setSeventDayNtvr(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics1.getSevenDayDrbVol().divide(sevenDayDrbVol, 4, RoundingMode.FLOOR).multiply(new BigDecimal("100"))));
                }
            }
        }

        canvasRankingsResVo.setTotalVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getTotalVol()));
        if (canvasDrbStatistics.getOwners() != null) {
            canvasRankingsResVo.setOwners(canvasDrbStatistics.getOwners());
        }
        if (canvasDrbStatistics.getNft() != null) {
            canvasRankingsResVo.setNfts(canvasDrbStatistics.getNft());
        }
        canvasRankingsResVo.setDaoStatus(canvasDrbStatistics.getDaoStatus());

        return canvasRankingsResVo;
    }
}
