package semios.api.model.vo.req.DaoExportInfoParam;

import lombok.Data;

/**
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 16:13
 **/
@Data
public class DaoExportParam {


    /**
     * daoId
     */
    private String daoId;

    /**
     * 1-sub node 2-template
     */
    private Integer type = 0;

    /**
     * @ignore
     */
    private String userAddress;
}
