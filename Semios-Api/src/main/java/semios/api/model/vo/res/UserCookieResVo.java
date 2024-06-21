package semios.api.model.vo.res;

import lombok.Data;


/**
 * @description: user profile
 * @author: xiangbin
 * @create: 2022-08-08 10:39
 **/
@Data
public class UserCookieResVo {

    /**
     * 用户名
     */
    private String name = "";

    /**
     * 用户头像图片地址
     */
    private String avatar = "";

    /**
     * 用户地址
     */
    private String address = "";

    /**
     * 用户token
     */
    private String token;

    /**
     * token剩余时间
     */
    private Long time = 0L;

    /**
     * 0-无权限 1-有权限
     */
    private Integer role;


}
