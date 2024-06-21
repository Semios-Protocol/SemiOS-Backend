package semios.api.model.vo.req;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.vo.RepeatVo;


/**
 * 修改canvas信息
 *
 * @description: dao create
 * @author: xiangbin
 * @create: 2022-08-10 14:31
 **/
@Data
public class CanvasEditReqVo extends RepeatVo {

    /**
     * canvas ID
     */
    private Integer canvasId;

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
     * @ignore
     */
    private String userAddress;


//==========1.5新增===========//

    /**
     * 社交链接 多个用逗号分隔
     *
     * @mock "http://123.com,http://456.com"
     */
    private String socialLinks;

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
     * canvas 的logo url
     *
     * @ignore
     */
    private String canvasLogoUrl;
}
