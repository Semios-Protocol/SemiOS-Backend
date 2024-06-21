package semios.api.model.vo.res;

import lombok.Data;

/**
 * D4A是否停机返回对象
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-12-02 13:48
 **/
@Data
public class D4APausedResVo {


    /**
     * D4A是否停机 0-未停机 1-已停机
     */
    private Integer d4aPaused = 0;

    /**
     * DAO是否停机 0-未停机 1-已停机
     */
    private Integer daoPaused = 0;

    /**
     * Canvas是否停机 0-未停机 1-已停机
     */
    private Integer canvasPaused = 0;
}
