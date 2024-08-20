package semios.api.model.vo.res.DaoExportInfo;

import lombok.Data;
import semios.api.model.entity.Dao;
import semios.api.utils.CommonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Data
public class NodeWorksParam {
    /**
     * 程序判断是否生成work的字段 添加work  0-不需要添加 1-需要添加
     */
    private Integer needMintableWork;

    /**
     * 设置自动生成work的总数量 0-为未设置
     */
    private Integer generateWorkSet;


    /**
     * 是否开启Erc20支付模式，开启时为1，关闭时为0。
     */
    private Integer erc20PaymentMode;

    /**
     * Unified ERC-721 Mint Fee Mode 是否开启全局一口价
     */
    private Integer globalDaoPriceMode;

    /**
     * dao全局铸造价格, 未开启全局一口价时，这个字段为 null
     */
    private BigDecimal globalDaoPrice = null;


    /**
     * DAO地板价 DAO Floor Price
     * 开启一口价后不用关注此字段
     */
    private BigDecimal daoFloorPrice;


    /**
     * ERC-721 Price Fluctuation Method
     * canvas价格变化规律 0-指数增加 1-线性增长
     * 开启一口价后不用关注此字段
     */
    private Integer canvasPriceFluctuationMethod;

    /**
     * canvas价格增长系数
     * 依赖于canvasPriceFluctuationMethod字段  对应priceFactor
     * 开启一口价后不用关注此字段
     */
    private BigDecimal fluctuationMethodFactor;


    public static NodeWorksParam transferNodeWorksParam(Dao dao) {
        NodeWorksParam nodeWorksParam = new NodeWorksParam();
        nodeWorksParam.setNeedMintableWork(dao.getNeedMintableWork() == 0 ? 1 : 0);   // 数据库0为需要添加，1为不需要添加
        nodeWorksParam.setGenerateWorkSet(dao.getGenerateWorkSet());
        nodeWorksParam.setErc20PaymentMode(dao.getErc20PaymentMode());

        nodeWorksParam.setGlobalDaoPriceMode(dao.getGlobalDaoPrice().compareTo(BigDecimal.ONE.negate()) == 0 ? 0 : 1);
        if (nodeWorksParam.getGlobalDaoPriceMode() == 1) {
            nodeWorksParam.setGlobalDaoPrice(dao.getGlobalDaoPrice());
        }

        nodeWorksParam.setDaoFloorPrice(dao.getDaoFloorPrice());
        nodeWorksParam.setCanvasPriceFluctuationMethod(dao.getCanvasPriceFluctuationMethod());

        if (nodeWorksParam.getCanvasPriceFluctuationMethod() == 0) {
            nodeWorksParam.setFluctuationMethodFactor(dao.getFluctuationMethodFactor().divide(BigDecimal.valueOf(10000), 1, RoundingMode.HALF_UP));
        } else {
            nodeWorksParam.setFluctuationMethodFactor(dao.getFluctuationMethodFactor().divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.HALF_UP));
        }


        return nodeWorksParam;
    }

}
