package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通
 */
@NoArgsConstructor
@AllArgsConstructor
public enum DaoWhiteListEnum {

    CLOSE(0, "未开通"),
    ADDRESS(1, "地址"),
    ERC721(2, "erc721"),
    ADDRESS_ERC721(3, "erc721和address");

    @Getter
    private Integer status;

    @Getter
    private String desc;
}
