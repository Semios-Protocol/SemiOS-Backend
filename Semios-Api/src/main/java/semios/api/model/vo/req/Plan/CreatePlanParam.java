package semios.api.model.vo.req.Plan;

import jdk.nashorn.internal.ir.annotations.Ignore;
import lombok.Data;

@Data
public class CreatePlanParam {
    // 其他信息用不用存储在uri中

    /**
     * projectId
     */
    private String projectId;

    /**
     * plan name
     *
     * @ignore
     */
    private String planName = "name";

    /**
     * plan logo
     *
     * @ignore
     */
    private String planLogoUrl = "logo";

    /**
     * plan开始的日期
     *
     * @mock 2024-05-01
     */
    private String planStartDate;

    /**
     * 1.4 duration 每一个mintableRound的持续时间，单位小时
     *
     * @mock 1
     */
    private Integer duration;

    /**
     * 1.8.1 用户自己输入的外部三方20地址
     *
     * @mock 1
     */
    private String customTokenAddress;

    /**
     * @ignore
     */
    @Ignore
    private String userAddress;
}
