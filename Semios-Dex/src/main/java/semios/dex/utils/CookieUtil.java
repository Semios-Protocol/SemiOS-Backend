package semios.dex.utils;


import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

/**
 * @description: cookie
 * @author: xiangbin
 * @create: 2022-08-12 14:23
 **/
public class CookieUtil {


    public static String getUserAddressFromCookie(HttpServletRequest request, String name) {
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if (name.equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        return null;
    }
}
