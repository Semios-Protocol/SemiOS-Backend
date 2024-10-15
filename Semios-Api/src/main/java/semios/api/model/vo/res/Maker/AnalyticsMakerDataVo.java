package semios.api.model.vo.res.Maker;

import lombok.Data;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class AnalyticsMakerDataVo {
    /**
     * 当天的makers数量
     * @mock 123
     */
    private Integer makersCount;

    /**
     * 横坐标日期
     * @mock 2024/08/09
     */
    private String date;
}
