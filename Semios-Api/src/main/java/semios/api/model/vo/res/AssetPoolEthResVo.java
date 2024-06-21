package semios.api.model.vo.res;

import lombok.Data;

import java.util.List;

/**
 * 折线图
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class AssetPoolEthResVo {

    /**
     * 时间
     *
     * @mock 1684483200, 1684486800
     */
    private List<Long> time;

    /**
     * 资金池内总金额
     *
     * @mock 0.01, 0.02
     */
    private List<String> totalAmount;

    /**
     * 资金池收入
     *
     * @mock 0.01, 0.02
     */
    private List<String> incomes;

    /**
     * 资金池支出
     *
     * @mock 0.01, 0.02
     */
    private List<String> costs;

    /**
     * 资金池变化量
     *
     * @mock 0.01, 0.02
     */
    private List<String> changes;

    /**
     * 资金池内总金额最大值
     * <p>
     * mock 0.02
     */
    private String maxTotalAmount = "0";

    /**
     * 资金池收入最大值
     * <p>
     * mock 0.00002
     */
    private String maxIncomes = "0";

    /**
     * 资金池支出最大值
     * <p>
     * mock 0.02
     */
    private String maxCosts = "0";

    /**
     * 资金池变化量最大值
     * <p>
     * mock 0.00002
     */
    private String maxChanges = "0";

}
