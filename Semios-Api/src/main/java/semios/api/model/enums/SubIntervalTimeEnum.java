package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum SubIntervalTimeEnum {

    TEN(10, "10s"), SIXTY(60, "60s");

    @Getter
    private Integer time;

    @Getter
    private String desc;
}
