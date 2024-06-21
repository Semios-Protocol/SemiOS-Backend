package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: dao project id
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoProjectVo {

    /**
     * dao的ID
     */
    private String projectId;

    /**
     * @ignore
     */
    private String userAddress;
}
