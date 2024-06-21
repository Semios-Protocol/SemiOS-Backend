package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 0-其他dao分配 1-当前dao分配 2-当前dao的redeem池分配
 *
 * @description: status
 * @author: xiangbin
 * @create: 2022-08-26 15:59
 **/
@NoArgsConstructor
@AllArgsConstructor
public enum DaoAllocationTypeEnum {
    OTHER_DAO(0, "其他dao分配"),
    CURRENT_DAO(1, "当前dao分配"),
    DAO_REDEEM(2, "当前dao的redeem池分配");
    @Getter
    private Integer type;

    @Getter
    private String desc;
}
