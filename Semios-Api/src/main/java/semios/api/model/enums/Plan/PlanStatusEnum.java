package semios.api.model.enums.Plan;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * plan status
 *
 * @description: basicType
 * @author: zhyyao
 * @create: 2024-05-01 15:59
 **/
@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum PlanStatusEnum {
    NOT_STARTED(1, "未开始"),
    STARTED(2, "已开始"),
    FINISHED(3, "已结束");


    private Integer basicType;

    private String daoDesc;
}
