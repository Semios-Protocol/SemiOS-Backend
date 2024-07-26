package semios.api.model.vo.res.NodePermission;

import lombok.Data;

/**
 * @description: dao id查询某操作的Address,Nft信息
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class SelectPermissionNft {

    /**
     * work图片地址
     */
    private String imgUrl;

    /**
     * 背景颜色 宽260，然后缩放后高度，然后加上背景色
     */
    private String bgColor;

    /**
     * nft高度 宽260之后的高度
     */
    private Double height;

    /**
     * work id 用来跳转nft详情页
     *
     * @mock 1
     */
    private Integer workId;

    /**
     * dao Name 用来拼接NFT名称
     *
     * @mock zhyyao
     */
    private String daoNameNft;

    /**
     * nft编号 用来拼接NFT名称,合约参数
     *
     * @mock 1
     */
    private Integer workNumber;

    /**
     * nft的erc721地址,合约参数
     *
     * @mock 0x20031867cc2cea0f06a3961bfc5e649880d4d8e0
     */
    private String erc721Token;

    /**
     * 该nft所拥有的权限数量
     *
     * @mock 10
     */
    private Integer permissionCount;

}
