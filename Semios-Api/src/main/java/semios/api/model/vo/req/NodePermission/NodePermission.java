package semios.api.model.vo.req.NodePermission;

import lombok.Data;

/**
 * @description: dao id查询某操作的Address,Nft信息
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class NodePermission {

    /**
     * dao id 或者 聚合dao id
     *
     * @required
     */
    private Integer daoId;

    /**
     * 权限类型
     * (sub dao 传值为1，2，3，4 中的一个)
     * (seed dao 传值为5，6，7 中的一个)
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
     * 当前登陆用户地址
     *
     * @ignore
     */
    private String userAddress;

}
