package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.entity.User;


/**
 * @description: user profile
 * @author: xiangbin
 * @create: 2022-08-08 10:39
 **/
@Data
public class UserProfileResVo {

    /**
     * 用户名
     */
    private String userName = "";

    /**
     * 用户介绍
     */
    private String introduction = "";

    /**
     * 用户头像图片
     */
    private MultipartFile avatar;


    /**
     * 用户头像图片地址
     */
    private String avatarLink = "";

    /**
     * 用户opensea地址
     */
    private String openseaLink = "";

    /**
     * 用户twitter地址
     */
    private String twitterLink = "";

    /**
     * 用户discord地址
     */
    private String discordLink = "";

    /**
     * 用户ID 不需要前端传值 传{}空对象即可
     */
    private String userAddress;

    public static UserProfileResVo transfer(User user) {
        UserProfileResVo userProfileVo = new UserProfileResVo();
        userProfileVo.setUserName(user.getUserName());
        userProfileVo.setIntroduction(user.getUserIntroduction());
        userProfileVo.setAvatarLink(user.getAvatarAddress());
        userProfileVo.setOpenseaLink(user.getOpenseaLink());
        userProfileVo.setTwitterLink(user.getTwitterLink());
        userProfileVo.setDiscordLink(user.getDiscordLink());
        userProfileVo.setUserAddress(user.getUserAddress());
        userProfileVo.setAvatar(null);
        return userProfileVo;
    }

    public static void transferToUser(UserProfileResVo userProfileVo, User user) {

        user.setUserName(userProfileVo.getUserName());
        user.setUserIntroduction(userProfileVo.getIntroduction());
        user.setAvatarAddress(userProfileVo.getAvatarLink());
        user.setOpenseaLink(userProfileVo.getOpenseaLink());
        user.setTwitterLink(userProfileVo.getTwitterLink());
        user.setDiscordLink(userProfileVo.getDiscordLink());

    }

    public void setUserName(String userName) {
        if (StringUtils.isNotBlank(userName)) {
            this.userName = userName;
        }
    }

    public void setIntroduction(String introduction) {
        if (StringUtils.isNotBlank(introduction)) {
            this.introduction = introduction;
        }
    }

    public void setAvatarLink(String avatarLink) {
        if (StringUtils.isNotBlank(avatarLink)) {
            this.avatarLink = avatarLink;
        }
    }

    public void setOpenseaLink(String openseaLink) {
        if (StringUtils.isNotBlank(openseaLink)) {
            this.openseaLink = openseaLink;
        }
    }

    public void setTwitterLink(String twitterLink) {
        if (StringUtils.isNotBlank(twitterLink)) {
            this.twitterLink = twitterLink;
        }
    }

    public void setDiscordLink(String discordLink) {
        if (StringUtils.isNotBlank(discordLink)) {
            this.discordLink = discordLink;
        }
    }

}
