package semios.api.model.vo.req;

import lombok.Data;

/**
 * 查询D4A停机参数
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-12-02 13:48
 **/
@Data
public class D4APausedReqVo {


    /**
     * daoId
     */
    private String daoId;

    /**
     * canvasId
     */
    private String canvasId;

}
