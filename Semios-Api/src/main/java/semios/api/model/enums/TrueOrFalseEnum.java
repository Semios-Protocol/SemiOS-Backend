package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum TrueOrFalseEnum {

    FALSE(0, false),
    TRUE(1, true);


    @Getter
    private Integer status;

    @Getter
    private Boolean name;
}
