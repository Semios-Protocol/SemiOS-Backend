package semios.dex.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author xiangbin
 */

@NoArgsConstructor
@AllArgsConstructor
public enum ContractMethodEnum {


    //查询token0数量 token0()
    PAIR_TOKE0("", "0x0dfe1681"),
    //查询token1数量 token1()
    PAIR_TOKE1("", "0xd21220a7"),
    //getReserves()
    GET_RESERVES("", "0x0902f1ac"),
    TOTAL_SUPPLY("", "0x18160ddd"),
    //balanceOf通用方法
    BALANCE_OF("", "0x70a08231"),

    // 根据project获取input token
    DAO_INPUT_TOKEN("", "0xbd2bf17f"),

    D4ATransfer("", "0x140bc2313a2670a47691808fc15120a7606016b5548f2a00cf24fda653989949"),
    D4AMint("", "0x653cdfdad74cc74920b2f8d7f5be1c9c468ea6f2476cfe5df6a4d67163fa3f85"),
    D4ASync("", "0x8b0d07dd4a894e7e06b7ad2e851b0561d78084f4a3c91b17677747bf6be13e52"),
    D4ABurn("", "0xdb69a77c7f00775bb8033e49ce1d856add6069898c697c0031e62069bce9018a"),
    D4ASwap("", "0x4012961728e0bbfc6a344936c7fe045feb896ffd93c7594258129a04fcd30e95"),
    DECIMALS("", "0x313ce567");

    @Getter
    private String contractAddress;

    @Getter
    private String methodAddress;
}
