package semios.api.model.vo.req;

import lombok.Data;
import semios.api.model.vo.PageVo;
import semios.api.model.vo.res.DaoSocialLinksVo;

import java.util.List;

/**
 * @description: work请求参数
 * @author: xiangbin
 * @create: 2022-08-05 15:42
 **/
@Data
public class WorkReqVo extends PageVo {

    /**
     * workId
     */
    private String workId;


    /**
     * work描述信息
     */
    private String workDescription;

    /**
     * @ignore
     */
    private String userAddress;


//    /**
//     * 社交链接 多个用逗号分隔
//     *
//     * @mock "http://123.com,http://456.com"
//     */
//    private String socialLinks;

    /**
     * 社交链接
     * <p>
     * 社交链接 List<DaoSocialLinks> 的json对象
     */
    private List<DaoSocialLinksVo> socialLinks;

    /**
     * opensea链接地址
     */
    private String openseaLink;

    /**
     * Twitter链接地址
     */
    private String twitterLink;

    /**
     * Discord链接地址
     */
    private String discordLink;

}
