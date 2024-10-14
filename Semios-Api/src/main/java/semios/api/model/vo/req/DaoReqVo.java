package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description: dao查询参数
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoReqVo extends PageVo {

    /**
     * dao的ID
     */
    private String daoId;

    /**
     * @ignore
     */
    private String userAddress;

    /**
     * 编辑dao为1，其余可以不传或传0
     */
    private Integer type;
}
