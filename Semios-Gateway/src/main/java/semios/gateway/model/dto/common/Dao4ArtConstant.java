package semios.gateway.model.dto.common;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @description: 常量
 * @author: xiangbin
 * @create: 2022-08-12 14:27
 **/
@Slf4j
@Component
public class Dao4ArtConstant {


    public static final String COOKIE_ADDRESS = "address";
    public static final String SESSION_ADDRESS = "user";
    public static final String COOKIE_TOKEN = "token";
    public static final String COOKIE_TOKEN_TIME = "time";
    public static final String COOKIE_ROLE = "role";
    public static final String COOKIE_USER_ADDRESS = "userAddress";
    public static final String COOKIE_NAME = "name";
    public static final String COOKIE_AVATAR = "avatar";
    public static final String LOCAL_HOST = "localhost";

    public static final String LOGIN_PATH_API = "/api/api/user/login";
    public static final String LOGIN_PATH = "/api/user/login";
    public static final String LOGOUT_PATH = "/api/user/logout";
}
