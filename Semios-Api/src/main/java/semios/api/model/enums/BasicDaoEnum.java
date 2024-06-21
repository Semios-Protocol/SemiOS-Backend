package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 是否为basic dao 1-proto dao 2- basic dao
 *
 * @description: basicType
 * @author: xiangbin
 * @create: 2022-08-26 15:59
 **/
@NoArgsConstructor
@AllArgsConstructor
public enum BasicDaoEnum {
    PROTO_DAO(1, "ProtoDao"),
    BASIC_DAO(2, "BasicDao");

    @Getter
    private Integer basicType;

    @Getter
    private String daoDesc;
}
