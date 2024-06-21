package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.RepeatVo;

import java.util.List;

/**
 * @description: delete work 参数
 * @author: xiangbin
 * @create: 2022-08-23 15:44
 **/
@Data
public class WorkDeleteReqVo extends RepeatVo {

    /**
     * canvas的Id
     */
//    private String canvasId;

    /**
     * 要删除的work的id
     */
    private List<String> workIds;

    /**
     * @Ignore
     */
    private String userAddress;
}
