package semios.api.model.vo.res.NodePermission;

import lombok.Data;

/**
 * @description: dao id查询某操作的Address,Nft信息
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class UserPermissionNft {

    /**
     * 权限类型
     * 1-SubNodes Edit Information Permission 编辑SubNode信息
     * 2-Edit On-chain Parameters Permission (work价格，mint window duration)
     * 3-Edit Strategies Permission
     * 4-Starter Reward Claim Permission 作为SubNodes Creator收益的权限
     * 5-Treasury Permission 国库的分配权限
     * 6-Seed Nodes Edit Information Permission 编辑seed nodes information的权限
     * 7-Top-Up Governance Permission 设置top-up账户分流权限
     */
    private Integer permissionType;

    /**
     * 权限归属的node名称 dao id ，用来跳转到node详情
     *
     * @mock 1
     */
    private String daoId;

    /**
     * 权限归属的node名称
     *
     * @mock zhyyao
     */
    private String daoName;

    /**
     * 权限归属的node project id
     *
     * @mock zhyyao
     */
    private String projectId;


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
