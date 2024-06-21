package semios.dex.model.vo.res;

import lombok.Data;

/**
 * @description: user
 * @author: xiangbin
 * @create: 2023-05-21 10:29
 **/
@Data
public class UserListResVo {

    /**
     * 用户地址
     */
    private String userAddress;

    /**
     * 用户名称
     */
    private String userName;

    /**
     * 用户logo
     */
    private String userLogo;

    /**
     * 是否为合约地址 1-是 0-否
     */
    private Integer isContract;


}
