package semios.api.model.vo.req.ExploreFilter;

import lombok.Data;

@Data
public class WorkPriceType {
    /**
     * 1.12 乐透模式开关
     * @mock true
     */
    private Boolean fixedPrice = null;

    /**
     * 1.12 是否开启Erc20支付模式 false-否 true-是
     * @mock true
     */
    private Boolean canvasPrice = null;

    /**
     * 1.12 top-up模式的开关
     * @mock true
     */
    private Boolean topupMode = null;
}
