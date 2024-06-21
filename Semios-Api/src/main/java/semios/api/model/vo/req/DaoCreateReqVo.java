package semios.api.model.vo.req;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jdk.nashorn.internal.ir.annotations.Ignore;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.vo.RepeatVo;


/**
 * 保存dao页面换取uri参数
 *
 * @description: dao create
 * @author: xiangbin
 * @create: 2022-08-10 14:31
 **/
@Data
public class DaoCreateReqVo extends RepeatVo {


    /**
     * dao名称
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
     * dao开始DRB
     */
    private String daoStartDate;

    /**
     * dao nft总量
     */
    private String totalNftCasting;

    /**
     * dao 总的drb数量
     */
    private String daoMintWindow;

    /**
     * dao地板价
     */
    private String daoFloorPrice;

    /**
     * dao创建手续费
     */
    private String daoCreateFee;

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
    @Ignore
    private String userAddress;
    /**
     * dao logo图片地址
     *
     * @ignore
     */
    private String daoLogoUrl;

    /**
     * dao banner图片地址
     *
     * @ignore
     */
    private String daoBgBannerUrl;


    /**
     * 社交链接 List<DaoSocialLinks>
     *
     * @mock [{"name":"link1","link":"http://123.com,http://456.com"},{"name":"link2","link":"http://123.com,http://236.com"}]
     */
    private String socialLinks;

}
