package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 1-白名单 2-erc721白名单 3-黑名单 4-高优白名单
 */
@NoArgsConstructor
@AllArgsConstructor
public enum DaoStrategyStrategyTypeEnum {
    WHITE_LIST(1, "白名单"),
    ERC721(2, "erc721白名单"),
    BLACK_LIST(3, "黑名单"),
    HIGH_PRIORITY(4, "高优白名单用户地址"),
    HIGH_PRIORITY_ERC721(5, "高优白名单ERC721地址"),
    ERC721_NFT(6, "erc721下的nft白名单"),
    HIGH_PRIORITY_ERC721_NFT(7, "高优白名单ERC721下的nft");

    @Getter
    private Integer type;

    @Getter
    private String desc;
}
