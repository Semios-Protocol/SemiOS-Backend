package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Objects;

/**
 * 零号 NFT描述枚举
 *
 * @description: ZeroNftDescriptionEnum
 * @author: zhyyao
 * @create: 2024-07-16 15:59
 **/
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum ZeroNftDescriptionEnum {
    // 不是main dao的情况下
    SubNode_Desc(0, "When you create a SubNodes in Semios, you will have the following 4 Permissions, which are:\n" +
            "1. SubNodes Edit Information Permission\n" +
            "2. Edit On-chain Parameters Permission\n" +
            "3. Edit Strategies Permission\n" +
            "4. Starter Reward Claim Permission\n" +
            "These permissions will be bound to the 0th NFT of the SubNodes ERC-721 contract. Whoever holds this NFT will have the above permissions. These permissions can also be bound to different NFTs (in My Permissions or on the details page of the corresponding NFT) allowing for more flexible management and customization of your Node's permissions."),

    // 在主页面创建dao，额外创建一个seed node
    SeedNode_Desc(1, "When you create a Seed Nodes in Semios, you will have the following 7 Permissions, which are\n" +
            "1. SubNodes Edit Information Permission\n" +
            "2. Edit On-chain Parameters Permission\n" +
            "3. Edit Strategies Permission\n" +
            "4. Starter Reward Claim Permission\n" +
            "5. Seed Nodes Edit Information Permission\n" +
            "6. Treasury Permission\n" +
            "7. Top-Up Governance Permission\n" +
            "These permissions will be bound to the 0th NFT of the SubNodes ERC-721 contract. Whoever holds this NFT will have the above permissions. These permissions can also be bound to different NFTs (in My Permissions or on the details page of the corresponding NFT) allowing for more flexible management and customization of your Node's permissions.");

    private Integer type;

    private String desc;


    public static String getDescByIsAncestordao(Integer type) {
        for (ZeroNftDescriptionEnum zeroNftDescriptionEnum : ZeroNftDescriptionEnum.values()) {
            if (Objects.equals(zeroNftDescriptionEnum.getType(), type)) {
                return zeroNftDescriptionEnum.getDesc();
            }
        }
        return "";
    }
}
