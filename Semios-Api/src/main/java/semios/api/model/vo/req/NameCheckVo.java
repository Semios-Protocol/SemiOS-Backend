package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: check name
 * @author: xiangbin
 * @create: 2022-09-08 15:34
 **/
@Data
public class NameCheckVo {

    /**
     * 名称
     */
    private String name;

    /**
     * 类型 0-dao 1-canvas 2-username
     */
    private String type;

    /**
     * dao id
     */
    private String daoId;

    /**
     * canvas id
     */
    private String canvasId;
}
