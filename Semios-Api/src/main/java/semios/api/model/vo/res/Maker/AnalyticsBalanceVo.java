package semios.api.model.vo.res.Maker;

import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class AnalyticsBalanceVo {
    /**
     * 总共的input或者output token数量
     * @mock 1
     */
    private BigDecimal totalToken;

    /**
     * input 或者 output token 的symbol
     * @mock 1
     */
    private String symbol;

    /**
     * input 或者 output token 的地址
     * @mock 0x0000000000000000000000000000000000000000
     */
    private String tokenAddress;

    /**
     * 图表中的数据列表
     * @mock [{"tokenBalance":123,"date":"2024/08/09"}]
     */
    private List<AnalyticsTokenDataVo> analyticsTokenDataVoList;
}
