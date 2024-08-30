package semios.api.model.vo.res.DaoExportInfo;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Dao;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Data
public class NodeBlockParam {

    /**
     * 返回当前的系统时间，此字段网页中不要使用，需要调用接口获取系统时间来处理
     */
    private String startDate;

    /**
     * 是否开启了TopUp模式 0-否 1-是
     */
    private Integer topupMode;


    /**
     * 是否开启无限模式，开启时返回1，关闭时返回0。
     */
    private Integer infiniteMode;

    /**
     * Nodes Block Window
     */
    private Integer daoMintWindow;


    /**
     * Nodes Block Window Duration
     */
    private Integer duration;


    /**
     * Total ERC-721 Mint Cap-最多可铸造的nft数量
     */
    private Integer totalNftCasting;


    /**
     * dao每天可以铸造的上限  最大值为10000
     */
    private Integer dailyMintCap;


    public static NodeBlockParam transferNodeBlockParam(Dao dao) {
        NodeBlockParam nodeBlockParam = new NodeBlockParam();
        nodeBlockParam.setStartDate(String.valueOf(System.currentTimeMillis() / 1000));
        nodeBlockParam.setTopupMode(dao.getTopupMode());
        nodeBlockParam.setInfiniteMode(dao.getInfiniteMode());
        nodeBlockParam.setDaoMintWindow(dao.getDaoMintWindow());
        nodeBlockParam.setDuration(new BigDecimal(dao.getDuration()).divide(new BigDecimal(ProtoDaoConstant.etherscanBlockNumber), 18, RoundingMode.UP).intValue());
        nodeBlockParam.setTotalNftCasting(dao.getTotalNftCasting());
        nodeBlockParam.setDailyMintCap(dao.getDailyMintCap());
        return nodeBlockParam;
    }
}
