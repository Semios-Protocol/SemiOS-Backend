package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 16:13
 **/
@Data
public class DaoIdParam extends PageVo {

    /**
     * 聚合daoId
     */
    private String daoId;

    /**
     * @ignore
     */
    private String userAddress;
}
