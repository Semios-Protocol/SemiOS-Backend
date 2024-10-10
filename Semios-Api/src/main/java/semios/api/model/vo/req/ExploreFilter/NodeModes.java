package semios.api.model.vo.req.ExploreFilter;

import lombok.Data;

@Data
public class NodeModes {
    /**
     * 1.12 乐透模式开关
     * @mock true
     */
    private Boolean lotteryMode = null;

    /**
     * 1.12 是否开启Erc20支付模式 false-否 true-是
     * @mock true
     */
    private Boolean erc20PaymentMode = null;

    /**
     * 1.12 top-up模式的开关
     * @mock true
     */
    private Boolean topupMode = null;

    /**
     * 1.12 全局统一一口价模式的开关
     * @mock true
     */
    private Boolean unifiedPriceMode = null;

    /**
     * 1.12 策略的设置
     * @mock true
     */
    private Boolean specialStrategy = null;

    /**
     * 1.12 是否开启了无限模式
     * @mock true
     */
    private Boolean infiniteMode = null;

}
