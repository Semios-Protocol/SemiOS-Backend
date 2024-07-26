package semios.api.model.vo.res.NodePermission;

import lombok.Data;

/**
 * @description: dao id查询某操作的Address,Nft信息
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class NodePermissionInfo {

    /**
     * 如果用户已登陆 表示登陆用户是否拥有该权限
     * true 拥有权限  false 没有权限
     */
    private Boolean isPermission;

    /**
     * Node 拥有这个权限的实际用户地址
     *
     * @mock 0x20031867cc2cea0f06a3961bfc5e649880d4d8e0
     */
    private String ownerAddress;


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
     * nft编号 用来拼接NFT名称
     *
     * @mock 1
     */
    private Integer workNumber;

    /**
     * nft的erc721地址
     *
     * @mock 0x20031867cc2cea0f06a3961bfc5e649880d4d8e0
     */
    private String erc721Token;

}
