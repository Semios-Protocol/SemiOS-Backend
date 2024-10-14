package semios.api.model.vo.req.Maker;

import lombok.Data;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class AnalyticsTokenParam {
    /**
     * 需要查询的daoId,聚合dao id或者sub dao id
     * @mock 1
     */
    private Integer daoId;

    /**
     * 需要查询的type类型, 1: input, 2: output
     * @mock 1
     */
    private Integer type;
}
