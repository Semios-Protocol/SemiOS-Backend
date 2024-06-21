package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.entity.User;

/**
 * @description: favorite
 * @author: xiangbin
 * @create: 2022-08-05 16:19
 **/
@Data
public class WorkFavoriteResVo {

    /**
     * 展示用户的UserName如果没有
     * 用Unnamed占位
     */
    private String userName;

    /**
     * 展示用户的钱包地址
     * 格式为0X...1234
     */
    private String userAddress;

    /**
     * 展示用户的头像如果用户
     * 没有上传头像则展示默认头像
     */
    private String userImage;

    public static WorkFavoriteResVo transfor(User user) {
        WorkFavoriteResVo workFavoriteResVo = new WorkFavoriteResVo();
        workFavoriteResVo.setUserName(user.getUserName());
        workFavoriteResVo.setUserAddress(user.getUserAddress().toLowerCase());
        workFavoriteResVo.setUserImage(user.getAvatarAddress());


        return workFavoriteResVo;
    }
}
