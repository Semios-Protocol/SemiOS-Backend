package semios.api.model.vo.res.Maker;

import lombok.Data;

import java.math.BigDecimal;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class AnalyticsTokenDataVo {
    /**
     * 总共的input或者output token数量
     * @mock 123
     */
    private BigDecimal tokenBalance;

    /**
     * 横坐标日期
     * @mock 2024/08/09
     */
    private String date;
}
