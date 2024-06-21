package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.enums.WorkStatusEnum;

/**
 * @description: canvas查询参数带排序的
 * @author: fjtan
 * @create: 2023-04-04 17:03
 **/
@Data
public class CanvasSortedReqVo extends SortReqVo {

    /**
     * canvas主键ID
     */
    private String canId;

    /**
     * 每页显示多少条
     */
    private Long pageSize = 10L;

    /**
     * 当前页数
     */
    private Long pageNo = 1L;

    /**
     * work状态0-已创建1-已铸造2-已失效
     *
     * @Ignore 忽略
     */
    private Integer workStatus = WorkStatusEnum.NOT_CAST.getStatus();

    /**
     * 当前drb，不需要前端传值
     *
     * @Ignore 忽略
     */
    private Integer currentDrb;

    /**
     * canvasId
     *
     * @Ignore 忽略
     */
    private String canvasId;
}
