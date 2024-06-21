package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.RepeatVo;

/**
 * @description: 收藏请求参数
 * @author: xiangbin
 * @create: 2022-08-11 15:37
 **/
@Data
public class FavoriteReqVo extends RepeatVo {

    /**
     * 0-收藏dao
     * 1-收藏canvas
     * 2-收藏work
     */
    private Integer type;

    /**
     * daoId或者canvasId或者workID
     */
    private Integer favoriteId;

    /**
     * 用户ID 不需要前端传值 传{}空对象即可
     */
    private String userAddress;

    /**
     * 签名hash
     */
    private String signatureHash;

    /**
     * 签名原文
     */
    private String originalText;


}
