package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.entity.Canvas;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.DaoStatusEnum;

/**
 * @description: my canvas 列表
 * @author: xiangbin
 * @create: 2022-08-05 14:21
 **/
@Data
public class CanvasNameListResVo {


    /**
     * canvasID
     */
    private String canvasId;

    /**
     * canvas名称
     */
    private String canvasName;

    /**
     * canvasNumber
     */
    private String canvasNumber;

    /**
     * daoNumber
     */
    private String daoNumber;

    /**
     * 是否停机 0-停机 1-未停机
     */
    private Integer isPaused;

    public static CanvasNameListResVo transfer(Canvas canvas) {
        CanvasNameListResVo canvasNameListResVo = new CanvasNameListResVo();
        canvasNameListResVo.setCanvasId(canvas.getId() + "");
        canvasNameListResVo.setCanvasName(canvas.getCanvasName());
        canvasNameListResVo.setCanvasNumber(canvas.getCanvasNumber() + "");
        canvasNameListResVo.setDaoNumber(canvas.getDaoNumber() + "");
        canvasNameListResVo.setIsPaused(canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus()) || canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 0 : 1);
        return canvasNameListResVo;
    }
}
