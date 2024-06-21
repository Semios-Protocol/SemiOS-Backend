package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public enum EnvEnum {
    DEV("dev", "开发环境"),
    TEST("test", "测试环境"),
    UTA("uta", "预发布环境"),
    PROD("prod", "生产环境");

    @Getter
    private String env;

    @Getter
    private String desc;
}
