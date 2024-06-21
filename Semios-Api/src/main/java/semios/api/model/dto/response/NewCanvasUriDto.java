package semios.api.model.dto.response;

import lombok.Data;
import semios.api.model.vo.req.CanvasCreateReqVo;

/**
 * @description: canvas uri
 * @author: xiangbin
 * @create: 2022-08-24 18:17
 **/
@Data
public class NewCanvasUriDto {
    /**
     * {
     * "name": "canvas66",
     * "description": "This canvas was created for testing.",
     * "logo": "https://image-1301365217.cos.ap-hongkong.myqcloud.com/img_work1.jpeg"
     * }
     */

    private String name;

    private String description;

    private String logo;

    /**
     * opensea链接
     */
    private String opensea_link;

    /**
     * twitter链接
     */
    private String twitter_link;

    /**
     * discord链接
     */
    private String discord_link;

    /**
     * 用户地址
     *
     * @Ignore
     */
    private String user_address;


    /**
     * 社交链接 多个用逗号分隔
     */
    private String socialLinks;


    public static NewCanvasUriDto transfer(CanvasCreateReqVo canvasCreateReqVo) {
        NewCanvasUriDto newCanvasUriDto = new NewCanvasUriDto();

        newCanvasUriDto.setName(canvasCreateReqVo.getCanvasName());
        newCanvasUriDto.setLogo(canvasCreateReqVo.getCanvasLogoUrl());
        newCanvasUriDto.setDescription(canvasCreateReqVo.getCanvasDescription());
        newCanvasUriDto.setDiscord_link(canvasCreateReqVo.getDiscordLink());
        newCanvasUriDto.setOpensea_link(canvasCreateReqVo.getOpenseaLink());
        newCanvasUriDto.setTwitter_link(canvasCreateReqVo.getTwitterLink());
        newCanvasUriDto.setUser_address(canvasCreateReqVo.getUserAddress());
        newCanvasUriDto.setSocialLinks(canvasCreateReqVo.getSocialLinks());
        return newCanvasUriDto;

    }
}
