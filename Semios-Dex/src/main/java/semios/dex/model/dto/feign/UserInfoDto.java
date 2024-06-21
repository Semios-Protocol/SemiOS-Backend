package semios.dex.model.dto.feign;

import lombok.Data;

/**
 * Dao erc20 info
 *
 * @author: fjtan
 * @create: 2023-05-21 12:14
 **/
@Data
public class UserInfoDto {

    /**
     * user address
     */
    private String userAddress;

    /**
     * user avatar address
     */
    private String avatarAddress;

    /**
     * 用户名  没有用户名或合约地址时显示地址除0x后大写前六位
     */
    private String userName;

}
