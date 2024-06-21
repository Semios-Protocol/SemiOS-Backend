package semios.api.model.vo.res;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * 折线图
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoFoldLineResVo {

    /**
     * 时间
     */
    private List<Long> times;
    /**
     * 总销售金额
     */
    private List<BigDecimal> volume;
    /**
     * 平均铸造价格
     */
    private List<BigDecimal> price;
    /**
     * 总销售金额最大值
     */
    private BigDecimal maxVolume = BigDecimal.ZERO;
    /**
     * 平均铸造价格最大值
     */
    private BigDecimal maxPrice = BigDecimal.ZERO;

}
