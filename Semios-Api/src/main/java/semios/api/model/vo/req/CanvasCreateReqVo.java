package semios.api.model.vo.req;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jdk.nashorn.internal.ir.annotations.Ignore;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.vo.RepeatVo;


/**
 * 根据canvas信息获取uri地址
 *
 * @description: canvas信息获取uri地址
 * @author: xiangbin
 * @create: 2022-08-10 14:40
 **/
@Data
public class CanvasCreateReqVo extends RepeatVo {

    /**
     * dao的id
     */
    private String daoId;

    /**
     * canvas的名称
     */
    private String canvasName;

    /**
     * canvas描述
     */
    private String canvasDescription;

    /**
     * canvas 的logo
     */
    @JsonIgnore
    private MultipartFile canvasLogo;

    /**
     * opensea链接
     */
    private String openseaLink;

    /**
     * twitter链接
     */
    private String twitterLink;

    /**
     * discord链接
     */
    private String discordLink;

    /**
     * 社交链接 多个用逗号分隔
     *
     * @mock "http://123.com,http://456.com"
     */
    private String socialLinks;


    /**
     * canvas 的logo url
     *
     * @ignore
     */
    private String canvasLogoUrl;

    /**
     * canvas 的uri
     *
     * @ignore
     */
    private String canvasUri;

    @Ignore
    private String userAddress;


    public static Canvas transfer(CanvasCreateReqVo canvasCreateReqVo, Dao dao) {
        Canvas canvas = new Canvas();
        canvas.setCanvasName(canvasCreateReqVo.getCanvasName());
        canvas.setCanvasDescription(canvasCreateReqVo.getCanvasDescription());
        canvas.setCanvasLogo(canvasCreateReqVo.getCanvasLogoUrl());
        canvas.setCanvasUri(canvasCreateReqVo.getCanvasUri());

        canvas.setDaoId(dao.getId());
        canvas.setProjectId(dao.getProjectId());
        canvas.setDaoNumber(dao.getDaoNumber());
        canvas.setDaoSymbol(dao.getDaoSymbol());
        canvas.setOwnerAddress(canvasCreateReqVo.getUserAddress().toLowerCase());
        canvas.setOpenseaLink(canvasCreateReqVo.getOpenseaLink());
        canvas.setTwitterLink(canvasCreateReqVo.getTwitterLink());
        canvas.setDiscordLink(canvasCreateReqVo.getDiscordLink());
//

        return canvas;

    }
}
