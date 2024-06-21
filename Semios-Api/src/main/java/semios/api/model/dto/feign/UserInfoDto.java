package semios.api.model.dto.feign;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.entity.User;
import semios.api.service.feign.ISubscriptionService;

import java.util.Random;

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
     * 用户名 没有用户名或合约地址时显示地址除0x后大写前六位
     */
    private String userName;

    /**
     * 是否为合约地址 1-是 0-否
     */
    private Integer isContract;

    public static UserInfoDto transfer(User user, String headImage, ISubscriptionService iSubscriptionService) {
        UserInfoDto userListResVo = new UserInfoDto();
        if (user.getId() != null) {
            userListResVo.setUserAddress(user.getUserAddress());
            userListResVo.setUserName(user.getUserName());
            userListResVo.setAvatarAddress(user.getAvatarAddress());
            userListResVo.setIsContract(0);
        } else {
            userListResVo.setUserAddress(user.getUserAddress());
            userListResVo.setUserName(user.getUserName());
            Random random = new Random();
            int i = random.nextInt(32) + 1;
            String avatar = String.format(headImage, i);
            userListResVo.setAvatarAddress(avatar);
            Result<Boolean> resultBoolean =
                    iSubscriptionService.ethGetCodeCheckUserAddress(ProtoDaoConstant.netWork, user.getUserAddress());
            if (resultBoolean != null && resultBoolean.getData()) {
                userListResVo.setIsContract(0);
            } else {
                userListResVo.setIsContract(1);
            }

        }
        return userListResVo;

    }

}
