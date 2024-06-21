package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

/**
 * @description: work修改详情
 * @author: xiangbin
 * @create: 2022-08-05 15:43
 **/
@Slf4j
@Data
public class WorkEditResVo {

    /**
     * workId
     */
    private String workId;


    /**
     * work描述信息
     */
    private String workDescription;


//    /**
//     * 社交链接 多个用逗号分隔
//     *
//     * @mock "http://123.com,http://456.com"
//     */
//    private List<String> socialLinks;

    /**
     * 社交链接
     * <p>
     * 社交链接 List<DaoSocialLinks> 的json对象
     */
    private List<DaoSocialLinksVo> socialLinks = new ArrayList<>();

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

    /**
     * Work状态0-已创建1-已铸造2-已失效
     */
    private Integer workStatus;

    /**
     * 当前owner地址
     */
    private String ownerAddress;


}
