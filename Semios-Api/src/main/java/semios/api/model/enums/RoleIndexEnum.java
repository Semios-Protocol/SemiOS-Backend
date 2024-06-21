package semios.api.model.enums;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import semios.api.utils.CommonUtil;

/**
 * 角色hash对应code
 */
@NoArgsConstructor
@AllArgsConstructor
public enum RoleIndexEnum {
    NO_HAVE_ROLE(0, "无角色"),
    OPERATION_ROLE(1, "0x20296b01d0b6bd176f0c1e29644934c0047abf080dae43609a1bbc09e39bafdb"),
    DEFAULT_ADMIN_ROLE(2, "0x0000000000000000000000000000000000000000000000000000000000000000"),
    DAO_ROLE(3, "0x3b5d4cc60d3ec3516ee8ae083bd60934f6eb2a6c54b1229985c41bfb092b2603"),
    PROJECT_ROLE(4, "0x99b4cb81d693045578a6795d2045a8e4a6dc2d789bd539397ae2ce05c6604599");

    @Getter
    private Integer index;
    @Getter
    private String hash;

    public static Integer getIndexByHash(String hash) {
        if (StringUtils.isBlank(hash)) {
            return NO_HAVE_ROLE.getIndex();
        }
        hash = CommonUtil.addHexPrefixIfNotExist(hash);
        for (RoleIndexEnum indexEnum : RoleIndexEnum.values()) {
            if (indexEnum.getHash().equals(hash)) {
                return indexEnum.getIndex();
            }
        }
        return NO_HAVE_ROLE.getIndex();
    }
}
