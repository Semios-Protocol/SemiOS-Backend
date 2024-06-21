package semios.api.model.vo.res.Plan;

import lombok.Data;

@Data
public class TogetherPlanVo {

    /**
     * seed nodes的projectId
     *
     * @mock 10
     */
    private String projectId;

    /**
     * seed nodes下plan的总数量
     *
     * @mock 10
     */
    private Long planTotal = 0L;

    /**
     * seed nodes下正在进行的plan
     *
     * @mock 3
     */
    private Long planOngoing = 0L;

    /**
     * seed nodes下已经结束的plan
     *
     * @mock 4
     */
    private Long planEnd = 0L;

    /**
     * seed nodes下未开始的plan
     *
     * @mock 3
     */
    private Long planNotStarted = 0L;

}
