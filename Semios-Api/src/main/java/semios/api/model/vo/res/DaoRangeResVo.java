package semios.api.model.vo.res;

import lombok.Data;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * range
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoRangeResVo {

    /**
     * 范围
     *
     * @mock 0-0.1ETH
     */
    private String range;

    /**
     * 总数量用
     *
     * @mock 20
     */
    private String amount;

    /**
     * 占比
     *
     * @mock 0.25
     */
    private BigDecimal ratio;

    /**
     * 排序
     */
    private Integer order;

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        DaoRangeResVo that = (DaoRangeResVo) o;
        return Objects.equals(range, that.range);
    }

    @Override
    public int hashCode() {
        return Objects.hash(range);
    }
}
