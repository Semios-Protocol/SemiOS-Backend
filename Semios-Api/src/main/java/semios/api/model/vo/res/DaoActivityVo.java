package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.utils.ProtoDaoCommonUtil;

import java.math.BigDecimal;

/**
 * @description: dao下activity
 * @author: xiangbin
 * @create: 2022-08-04 18:47
 **/
@Data
public class DaoActivityVo {

    /**
     * DAO创建后的每一个区块名 不包括当前区块和轮空的区块
     */
    private Integer daoRebaseBlock;

    /**
     * DAO在该区块结束时所有canvas的最低价
     */
    private Float floorPrice = 0.0f;


    /**
     * DAO在该区块和之前的六个区块（包括轮空的区块）铸造费用总和
     */
    private Float sevenDayRdbVol = 0.0f;


    /**
     * DAO在该区块结束时资金池里的总金额
     */
    private Float daoAssetPool = 0.0f;

    /**
     * DAO在该区块结束时所发放的所有ERC20数量
     */
    private Long daoReward = 0L;

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
     * Mint Revenue+未来收益
     */
    private Float dre = 0.0f;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    public static DaoActivityVo transfer(DaoDrbStatistics daoDrbStatistics) {
        DaoActivityVo daoActivityVo = new DaoActivityVo();
        if (daoDrbStatistics.getDaoReward() != null) {
            daoActivityVo.setDaoReward(daoDrbStatistics.getDaoReward().longValue());
        }
        daoActivityVo.setDaoAssetPool(ProtoDaoCommonUtil.bigdecimalToFloat(daoDrbStatistics.getDaoAssetPool()));
        daoActivityVo.setDaoRebaseBlock(daoDrbStatistics.getDrbNumber());
        daoActivityVo.setCanvasNumber(daoDrbStatistics.getCanvas());
        if (StringUtils.isNotBlank(daoDrbStatistics.getDre())) {
            daoActivityVo.setDre(ProtoDaoCommonUtil.bigdecimalToFloat(new BigDecimal(daoDrbStatistics.getDre())));
        }
        if (daoDrbStatistics.getFloorPrice() != null) {
            daoActivityVo.setFloorPrice(ProtoDaoCommonUtil.bigdecimalToFloat(daoDrbStatistics.getFloorPrice()));
        }

        if (StringUtils.isNotBlank(daoDrbStatistics.getOwners())) {
            daoActivityVo.setOwnersNumber(Integer.valueOf(daoDrbStatistics.getOwners()));
        }
        if (StringUtils.isNotBlank(daoDrbStatistics.getNft())) {
            daoActivityVo.setNftNumber(Integer.valueOf(daoDrbStatistics.getNft()));
        }
        daoActivityVo.setSevenDayRdbVol(ProtoDaoCommonUtil.bigdecimalToFloat(daoDrbStatistics.getSevenDayDrbVol()));

        return daoActivityVo;
    }

    public void setFloorPrice(Float floorPrice) {
        if (floorPrice != null) {
            this.floorPrice = floorPrice;
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

    public void setDaoReward(Long daoReward) {
        if (daoReward != null) {
            this.daoReward = daoReward;
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
        this.nftNumber = nftNumber;
    }

    public void setDre(Float dre) {
        if (dre != null) {
            this.dre = dre;
        }
    }

}
