package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;

/**
 * @description: user
 * @author: xiangbin
 * @create: 2022-08-25 13:55
 **/
@Data
public class UserProfilePageReqVo extends PageVo {
    /**
     * 用户ID 不需要前端传值 传{}空对象即可
     */
    private String userAddress;
}
