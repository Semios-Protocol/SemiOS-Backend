package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description: work请求参数
 * @author: xiangbin
 * @create: 2022-08-05 15:42
 **/
@Data
public class WorkId extends PageVo {

    /**
     * workId
     */
    private String workId;
}
