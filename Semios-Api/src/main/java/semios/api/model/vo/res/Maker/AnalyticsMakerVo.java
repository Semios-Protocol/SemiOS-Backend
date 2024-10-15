package semios.api.model.vo.res.Maker;

import lombok.Data;

import java.util.List;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class AnalyticsMakerVo {
    /**
     * 总共的maker数量
     * @mock 1
     */
    private Integer totalMakers;

    /**
     * 图表中的数据列表
     * @mock [{"makersCount":123,"date":"2024/08/09"}]
     */
    private List<AnalyticsMakerDataVo> analyticsMakerDataVos;
}
