package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description: dao查询参数
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoCanvasExistsReqVo extends PageVo {

    /**
     * dao的ID
     */
    private String daoId;

    /**
     * workId
     */
    private String workId;

    /**
     * @ignore
     */
    private String userAddress;
}
