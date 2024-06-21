package semios.api.model.dto.response;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.Data;
import org.apache.commons.lang.time.DateFormatUtils;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.enums.DaoCreateFeeEnum;
import semios.api.model.vo.req.BasicDaoCreateReqVo;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.vo.req.DaoCreateReqVo;

import java.util.Date;

/**
 * @description: project的uri字段
 * @author: xiangbin
 * @create: 2022-08-24 15:53
 **/
@Data
public class NewProjectUriDto {
    /**
     * {
     * 	"name": "DAO8",
     * 	"manitesto": "This is manitesto of DAO8.",
     * 	"description": "This DAO was created for testing.",
     * 	"logo": "https://image-1301365217.cos.ap-hongkong.myqcloud.com/dao_logo.png",
     * 	"bg_banner": "https://image-1301365217.cos.ap-hongkong.myqcloud.com/dao_bg_banner.jpeg",
     * 	"seller_fee_basis_points": 750,
     * 	"fee_recipient": "0xeE7536A68f24243567201172A0E321703ee6Cd5F"
     * }
     */
    /**
     * project名称
     */
    private String name;
    /**
     * project宣言
     */
    private String manitesto;
    /**
     * 描述
     */
    private String description;
    /**
     * logo
     */
    private String logo;
    /**
     * 背景图片
     */
    private String bg_banner;
    /**
     * 卖方费用点数
     */
    private Integer seller_fee_basis_points;
    /**
     * fee的接收人地址
     */
    private String fee_recipient;

    //json 序列化 去空 https://blog.csdn.net/yandao/article/details/109575570
    /**
     * uri地址
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String uri;

    /**
     * opensea链接
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String opensea_link;

    /**
     * twitter链接
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String twitter_link;

    /**
     * discord链接
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String discord_link;

    /**
     * 用户地址
     *
     * @Ignore
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String user_address;

    /**
     * DAO开始日期
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String daoStart_date;

    /**
     * 社交链接 List<DaoSocialLinks>
     */
    @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
    private String socialLinks;


    public static NewProjectUriDto transfer(DaoCreateReqVo daoCreateReqVo, boolean md5) {
        NewProjectUriDto newProjectUriDto = new NewProjectUriDto();
        if (StringUtils.isNotBlank(daoCreateReqVo.getDaoName())) {
            newProjectUriDto.setName("DAO4Art " + daoCreateReqVo.getDaoName().trim());
        }
        newProjectUriDto.setManitesto(daoCreateReqVo.getDaoManitesto());
        newProjectUriDto.setDescription(daoCreateReqVo.getDaoDescription());
        newProjectUriDto.setLogo(daoCreateReqVo.getDaoLogoUrl());
        newProjectUriDto.setBg_banner(daoCreateReqVo.getDaoBgBannerUrl());
        newProjectUriDto.setOpensea_link(daoCreateReqVo.getOpenseaLink());
        newProjectUriDto.setTwitter_link(daoCreateReqVo.getTwitterLink());
        newProjectUriDto.setDiscord_link(daoCreateReqVo.getDiscordLink());
        newProjectUriDto.setUser_address(daoCreateReqVo.getUserAddress().toLowerCase());
        newProjectUriDto.setDaoStart_date(daoCreateReqVo.getDaoStartDate());
        newProjectUriDto.setSocialLinks(daoCreateReqVo.getSocialLinks());
        DaoCreateFeeEnum daoCreateFeeEnum = DaoCreateFeeEnum.getDaoCreateFeeEnumByIndex(Integer.parseInt(daoCreateReqVo.getDaoCreateFee()));
        if (daoCreateFeeEnum != null) {
            newProjectUriDto.setSeller_fee_basis_points(daoCreateFeeEnum.getActualValue() + ProtoDaoConstant.MINT_D4A_FEE_RATIO);
        }

        return newProjectUriDto;
    }

    public static NewProjectUriDto transfer(BasicDaoCreateReqVo bacisDaoCreateReqVo) {
        NewProjectUriDto newProjectUriDto = new NewProjectUriDto();
        if (StringUtils.isNotBlank(bacisDaoCreateReqVo.getDaoName())) {
            newProjectUriDto.setName("ProtoDao " + bacisDaoCreateReqVo.getDaoName().trim());
        }

        newProjectUriDto.setUser_address(bacisDaoCreateReqVo.getUserAddress().toLowerCase());
        if (StringUtils.isNotBlank(bacisDaoCreateReqVo.getDaoStartDate())) {
            newProjectUriDto.setDaoStart_date(bacisDaoCreateReqVo.getDaoStartDate());
        } else {
            newProjectUriDto.setDaoStart_date(DateFormatUtils.format(new Date(), "yyyy-MM-dd"));
        }
        DaoCreateFeeEnum daoCreateFeeEnum = DaoCreateFeeEnum.ZERO;
        newProjectUriDto.setSeller_fee_basis_points(daoCreateFeeEnum.getActualValue() + ProtoDaoConstant.MINT_D4A_FEE_RATIO);

        return newProjectUriDto;
    }


}
