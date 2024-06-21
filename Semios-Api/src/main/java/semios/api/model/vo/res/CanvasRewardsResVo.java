package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.utils.ProtoDaoCommonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @description: rewards
 * @author: xiangbin
 * @create: 2022-08-05 15:29
 **/
@Data
public class CanvasRewardsResVo {

    /**
     * Canvas创建后的每一个区块名
     * 不包括当前区块和DAO轮空的区块
     */
    private Integer daoRebseBlock;

    /**
     * Canvas在该区块结束时的价格
     */
    private Float mintPrice = 0.0f;

    /**
     * Canvas在当前区块铸造费用总和
     */
    private Float drbVol = 0.0f;

    /**
     * Canvas在本区块和之前所有区块铸造总费用
     */
    private Float totalVol = 0.0f;

    /**
     * Canvas在本区块铸造总费用占DAO在本区块铸造总费用的占比
     */
    private Float ntvr = 0.0f;

    /**
     * Canvas在该区块结束后累计收到的ERC20数量
     */
    private Long daoReward = 0L;

    /**
     * DRE下的数据为Mint Revenue+未来收益
     */
    private Float dre = 0.0f;

    /**
     * Canvas在该区块结束时所有NFT所在地址的私钥拥有者去重计数
     */
    private Integer owners = 0;

    /**
     * Canvas在该区块结束时所有NFT的数量
     */
    private Integer nfts = 0;

    public static CanvasRewardsResVo transfer(CanvasDrbStatistics canvasDrbStatistics) {
        CanvasRewardsResVo canvasRewardsResVo = new CanvasRewardsResVo();
        canvasRewardsResVo.setDaoRebseBlock(canvasDrbStatistics.getDrbNumber());
        canvasRewardsResVo.setMintPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getMintPrice()));
        canvasRewardsResVo.setDrbVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getDrbVol()));
        canvasRewardsResVo.setTotalVol(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getTotalVol()));
        canvasRewardsResVo.setNtvr(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getNtvr()));
        if (canvasDrbStatistics.getDaoReward() != null) {
            canvasRewardsResVo.setDaoReward(canvasDrbStatistics.getDaoReward().longValue());
        }
        canvasRewardsResVo.setDre(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getDre()));
        canvasRewardsResVo.setOwners(canvasDrbStatistics.getOwners());
        canvasRewardsResVo.setNfts(canvasDrbStatistics.getNft());

        return canvasRewardsResVo;
    }

    public void setMintPrice(Float mintPrice) {
        if (mintPrice != null) {
            this.mintPrice = mintPrice;
        }
    }

    public void setDrbVol(Float drbVol) {
        if (drbVol != null) {
            this.drbVol = drbVol;
        }
    }

    public void setTotalVol(Float totalVol) {
        if (totalVol != null) {
            this.totalVol = totalVol;
        }
    }

    public void setNtvr(Float ntvr) {
        if (ntvr != null) {
            this.ntvr = new BigDecimal(ntvr).multiply(new BigDecimal("100")).setScale(4, RoundingMode.FLOOR).stripTrailingZeros().floatValue();
        }
    }

    public void setDaoReward(Long daoReward) {
        if (daoReward != null) {
            this.daoReward = daoReward;
        }
    }

    public void setDre(Float dre) {
        if (dre != null) {
            this.dre = dre;
        }
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

}
