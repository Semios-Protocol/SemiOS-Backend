package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum NameCheckTypeEnum {
    DAO("0", "dao"),
    CANVAS("1", "canvas"),
    USER_NAME("2", "user name");

    @Getter
    private String type;

    @Getter
    private String desc;
}
