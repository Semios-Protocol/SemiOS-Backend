package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description: canvas请求参数
 * @author: xiangbin
 * @create: 2022-08-05 14:51
 **/
@Data
public class CanvasReqVo extends PageVo {

    /**
     * canvas Id
     *
     * @required true
     * @mock 10
     */
    private String canvasId;


    /**
     * @ignore
     */
    private String userAddress;

    /**
     * 价格类型 0-canvas_price 1-fixed_price
     */
    private Integer fixedPrice;

}
