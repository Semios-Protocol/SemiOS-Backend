package semios.dex.model.vo.res;

import lombok.Data;

/**
 * Trending Royalty Token
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class TrendingResVo {

    /**
     * erc20地址
     *
     * @mock 0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String erc20Address;

    /**
     * daoId
     */
    private String daoId;

    /**
     * dao name
     *
     * @mock Crypto Jogger
     */
    private String daoName;

    /**
     * dao logo
     *
     * @mock https://image.dao4.art/dao/D16803917761033913.png
     */
    private String daoLogoUrl;

    /**
     * ERC20 Symbol
     *
     * @mock D4A.T122
     */
    private String erc20Symbol;

    /**
     * ERC20在DEX中的价格
     *
     * @mock 0.01
     */
    private String price = "0";

    /**
     * ERC20价格24小时内的变化量 公式为：（24小时前的价格-当前价格）/24小时前的价格*-1*100%
     * 创建时间不足24小时的用绿色箭头和--表示
     *
     * @mock 0.1234
     */
    private String oneDayChanged = "--";

    /**
     * ERC20价格七天的变化量 公式为：（7天前的价格-当前价格）/7天前的价格*-1*100%
     * 创建时间不足7天的用绿色箭头和--表示
     *
     * @mock 0.1234
     */
    private String sevenDayChanged = "--";

    /**
     * ERC20在24小时内swap交易中ETH的总和
     *
     * @mock 456
     */
    private String oneDayVolume = "0";

    /**
     * 在24小时内swap交易中ETH的总和的交易变化量
     * 公式为：48小时前到24小时前的交易量-24小时内的交易量）/48小时前到24小时前的交易量*-1*100%
     *
     * @mock 0.1234
     */
    private String oneDayVolumeChanged = "--";

    /**
     * ERC20当前的市值
     * 公式为：当前价格乘流通量
     *
     * @mock 1000.001
     */
    private String marketCap = "0";

    /**
     * ERC20的版税收益(二次交易的收益)
     *
     * @mock 10.001
     */
    private String royaltyFee = "0";

    /**
     * feePool地址
     */
    private String feePool;

}
