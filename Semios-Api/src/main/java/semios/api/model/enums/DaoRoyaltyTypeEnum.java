package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 分配类型 0-非当前dao 1-redeem Asset Pool 2-selfReward 3-不出块比例
 *
 * @author xiangbin
 */
@NoArgsConstructor
@AllArgsConstructor
public enum DaoRoyaltyTypeEnum {

    ZERO(0, "非当前dao"),
    ONE(1, "Seed Nodes Redeem Pool"), // old: Redem Asset Pool
    TWO(2, "This Nodes Internal Incentives"),  // old: Internal Reward
    THREE(3, "This Nodes Reserves");//不出块比例 // old: Retained Proportion

    @Getter
    private Integer type;
    @Getter
    private String desc;


    public static DaoRoyaltyTypeEnum getEnumByType(Integer type) {
        for (DaoRoyaltyTypeEnum daoRoyaltyTypeEnum : DaoRoyaltyTypeEnum.values()) {
            if (daoRoyaltyTypeEnum.getType().equals(type)) {
                return daoRoyaltyTypeEnum;
            }
        }
        return null;
    }

}