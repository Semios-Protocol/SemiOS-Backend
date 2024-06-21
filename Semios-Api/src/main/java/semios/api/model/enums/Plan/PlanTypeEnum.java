package semios.api.model.enums.Plan;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * plan的激励类型 1-input token 2-output token
 *
 * @description: basicType
 * @author: zhyyao
 * @create: 2024-05-01 15:59
 **/
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum PlanTypeEnum {
    INPUT_TOKEN(1, "input token"),
    OUTPUT_TOKEN(2, "output token");

    private Integer basicType;

    private String daoDesc;
}
