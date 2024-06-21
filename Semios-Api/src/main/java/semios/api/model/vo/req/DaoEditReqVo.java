package semios.api.model.vo.req;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.vo.RepeatVo;


/**
 * 修改dao信息
 *
 * @description: dao create
 * @author: xiangbin
 * @create: 2022-08-10 14:31
 **/
@Data
public class DaoEditReqVo extends RepeatVo {

    /**
     * canvas ID
     */
    private Integer canvasId;


    /**
     * dao ID
     */
    private Integer daoId;

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

    /**
     * 签名hash
     */
    private String signatureHash;

    /**
     * 签名原文
     */
    private String originalText;

//==========1.5新增===========//
    /**
     * dao名称
     * 由于无法刷新opensea 暂时禁止修改 聚合dao可以修改
     */
    private String daoName;

    /**
     * dao宣言
     */
    private String daoManitesto;

    /**
     * dao描述
     */
    private String daoDescription;

    /**
     * dao logo图片
     */
    @JsonIgnore
    private MultipartFile daoLogo;

    /**
     * dao banner图片
     */
    @JsonIgnore
    private MultipartFile daoBgBanner;

    /**
     * 社交链接 多个用逗号分隔
     *
     * @mock {"link1":"http://www.baidu.com","link2":"http://www.baidu123.com","link3":"http://www.baidu1234.com"}
     */
    private String socialLinks;

    /**
     * @ignore
     */
    @JsonIgnore
    private String s3DaoLogoUrl;
    /**
     * @ignore
     */
    @JsonIgnore
    private String s3DaoBgBannerUrl;

    @JsonIgnore
    private String oldDaoWorkUrl;

}
