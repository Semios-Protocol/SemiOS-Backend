package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: work请求参数
 * @author: xiangbin
 * @create: 2022-08-05 15:42
 **/
@Data
public class WorkIdReqVo {

    /**
     * workId
     */
    private String workId;

    /**
     * @ignore
     */
    private String userAddress;


}
