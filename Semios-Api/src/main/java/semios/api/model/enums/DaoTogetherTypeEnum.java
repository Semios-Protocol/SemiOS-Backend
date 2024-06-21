package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public enum DaoTogetherTypeEnum {
    NOT_TOGETHER_DAO(0, "非聚合dao"),
    IS_TOGETHER_DAO(1, "聚合dao");


    private Integer status;

    private String name;
}
