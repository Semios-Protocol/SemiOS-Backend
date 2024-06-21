package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 16:13
 **/
@Data
public class DaoIdReqVo {


    /**
     * daoId
     */
    private String daoId;

    /**
     * 1-create canvas 2-minting
     */
    private Integer type;

    /**
     * @ignore
     */
    private String userAddress;
}
