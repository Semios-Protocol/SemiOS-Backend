package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * NFT 权限类型枚举
 *
 * @description: NodePermissionTypeEnum
 * @author: zhyyao
 * @create: 2024-07-16 15:59
 **/
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum NodePermissionTypeEnum {
    Edit_SubNode(1, "SubNodes Edit Information Permission"),    // 编辑SubNode信息
    Edit_OnChain(2, "Edit On-chain Parameters Permission"),    // Edit On-chain parameters权限（work价格，mint window duration.....)
    Edit_Strategies(3, "Edit Strategies Permission"),  // Edit Strategies权限
    Proceeds_Ratio(4, "Starter Reward Claim Permission"), // 作为SubNodes Creator收益的权限

    Treasury_Allocation(5, "Treasury Permission"),  // 国库的分配权限
    Edit_SeedNode(6, "Seed Nodes Edit Information Permission"), // 编辑seed nodes information的权限
    Topup_Ratio(7, "Top-Up Governance Permission"); // 设置top-up账户分流权限

    private Integer type;

    private String permissionDesc;
}
