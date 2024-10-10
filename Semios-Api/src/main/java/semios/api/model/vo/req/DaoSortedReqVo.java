package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.req.ExploreFilter.BaseFilter;

import java.util.List;

/**
 * @description: dao查询参数带排序的
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoSortedReqVo extends BaseFilter {

    /**
     * dao的ID
     */
    private String daoId;

    /**
     * 每页显示多少条
     */
    private Long pageSize = 10L;

    /**
     * 当前页数
     */
    private Long pageNo = 1L;

    /**
     * 当前drb，不需要前端传值
     *
     * @ignore 忽略
     */
    private Integer currentDrb;

    /**
     * projectId
     *
     * @ignore 忽略
     */
    private String projectId;

    /**
     * erc20Token
     *
     * @ignore 忽略
     */
    private String erc20Token;

    /**
     * 根据指定的id查询
     *
     * @ignore 忽略
     */
    private List<String> daoIdList;
}
