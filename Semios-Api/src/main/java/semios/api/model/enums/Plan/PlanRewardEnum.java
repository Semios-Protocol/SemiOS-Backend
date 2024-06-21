package semios.api.model.enums.Plan;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * plan的发方奖励token类型 1-input  2-output 3-custom
 *
 * @description: basicType
 * @author: zhyyao
 * @create: 2024-05-01 15:59
 **/
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum PlanRewardEnum {
    INPUT_TOKEN(1, "input token"),
    OUTPUT_TOKEN(2, "output token"),
    CUSTOM_TOKEN(3, "output token");

    private Integer basicType;

    private String daoDesc;
}
