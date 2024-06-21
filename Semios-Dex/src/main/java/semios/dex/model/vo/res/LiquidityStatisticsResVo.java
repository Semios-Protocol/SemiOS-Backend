package semios.dex.model.vo.res;

import lombok.Data;

/**
 * 流通性统计信息
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class LiquidityStatisticsResVo {

    /**
     * 7 Days Volume 7天swap交易的eth总和
     *
     * @mock 12
     */
    private String sevenDaysVolume = "0";

    /**
     * swap交易中ETH的交易总和
     *
     * @mock 13
     */
    private String totalVolume = "0";

    /**
     * 市场价值-当前价格乘流通量
     *
     * @mock 14
     */
    // private String marketCap = "0";

    /**
     * 当前价格乘最大流通量
     *
     * @mock 15
     */
    // private String fullyDilutedMarketCap = "0";

    /**
     * 已发放的数量减Burn的数量
     *
     * @mock 16
     */
    private String circulatingSupply = "0";

    /**
     * ERC20最大数量10亿个
     *
     * @mock 1000000000
     */
    private String totalSupply = "0";

    /**
     * ERC20已经Burn的数量
     *
     * @mock 17
     */
    private String burnVolume = "0";

    /**
     * 最大数量减去Burn的数量
     *
     * @mock 18
     */
    private String valuableSupply = "0";

}
