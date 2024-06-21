package semios.subscription.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum NetWorkEnum {
    Mainnet("mainnet", "mainnet", "0"),
    Ropsten("ropsten", "ropsten", "1"),
    Rinkeby("rinkeby", "rinkeby", "2"),
    Goerli("goerli", "goerli", "3"),
    Sepolia("sepolia", "sepolia", "4");

    @Getter
    private String name;
    @Getter
    private String value;

    @Getter
    private String id;

    public static NetWorkEnum getByName(String name) {
        for (NetWorkEnum workEnum : NetWorkEnum.values()) {
            if (workEnum.getName().equalsIgnoreCase(name)) {
                return workEnum;
            }
        }
        return null;
    }

}
