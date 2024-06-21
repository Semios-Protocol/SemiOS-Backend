package semios.api.model.bo;

import lombok.Data;

/**
 * @description: DaoAnalyticsReqparam
 * @author: xiangbin
 * @create: 2023-04-20 11:07
 **/
@Data
public class DaoAnalyticsBo {

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * 开始时间
     */
    private String startDate;

    /**
     * 结束时间
     */
    private String endDate;

    /**
     * work铸造状态
     */
    private Integer workStatus;

    /**
     * dao 的 projectId
     */
    private String projectId;

}
