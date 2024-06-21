package semios.api.model.enums.Plan;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * plan的奖励来源方式
 * 奖励来源 1-国库 2-钱包
 *
 * @description: basicType
 * @author: zhyyao
 * @create: 2024-05-01 15:59
 **/
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum PlanAmountSourceEnum {
    TREASURY(1, "treasury"),
    WALLET(2, "wallet");

    private Integer basicType;

    private String daoDesc;
}
