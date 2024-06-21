package semios.api.model.entity;

import java.io.Serializable;
import java.time.LocalDateTime;

import org.apache.commons.lang3.StringUtils;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;

/**
 * <p>
 * 用户信息表
 * </p>
 *
 * @author xiangbin
 */
@Data
public class User implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * 用户地址
     */
    private String userAddress;

    /**
     * 用户昵称
     */
    private String userName;

    /**
     * 用户个人介绍
     */
    private String userIntroduction;

    /**
     * 头像地址
     */
    private String avatarAddress;

    /**
     * opensea链接
     */
    private String openseaLink;

    /**
     * Twitter链接
     */
    private String twitterLink;

    /**
     * discord链接
     */
    private String discordLink;

    /**
     * 首次登陆时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime firstLoginTime;

    /**
     * 是否签署隐私协议0-未签署1-已签署
     */
    private Integer signPrivacyAgreement;

    /**
     * 用户权限0-无权限 1-OPERATION_ROLE 2-DEFAULT_ADMIN_ROLE 3-DAO_ROLE 4-PROJECT_ROLE
     */
    private Integer role;

    /**
     * 交易Hash
     */
    private String transactionHash;

    /**
     * 是否为合约地址 0-否 1-是
     */
    private Integer isContract;

    public String getAvatarAddress() {
        if (StringUtils.isBlank(avatarAddress)) {
            return avatarAddress;
        }
        return avatarAddress.replace(ProtoDaoConstant.s3ImageUrl, ProtoDaoConstant.s3CdnImageUrl);
    }

}
