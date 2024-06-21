package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.RepeatVo;

/**
 * @Ignore
 * @description: profile
 * @author: xiangbin
 * @create: 2022-08-08 10:37
 **/

@Data
public class UserProfileReqVo extends RepeatVo {
    /**
     * 用户ID 不需要前端传值 传{}空对象即可
     *
     * @ignore
     */
    private String userAddress;
}
