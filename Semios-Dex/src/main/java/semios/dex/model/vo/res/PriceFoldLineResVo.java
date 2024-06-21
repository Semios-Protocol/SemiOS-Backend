package semios.dex.model.vo.res;

import lombok.Data;

import java.util.List;

/**
 * 价格折线图
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class PriceFoldLineResVo {

    /**
     * 时间
     *
     * @mock 1684483200, 1684486800
     */
    private List<Long> time;

    /**
     * 交易量
     *
     * @mock 0.01, 0.02
     */
    private List<String> volume;

    /**
     * 兑换比例
     *
     * @mock 0.00001, 0.00002
     */
    private List<String> price;

    /**
     * 交易量最大值
     * <p>
     * mock 0.02
     */
    private String maxVolume = "0";

    /**
     * 价格最大值
     * <p>
     * mock 0.00002
     */
    private String maxPrice = "0";

}
