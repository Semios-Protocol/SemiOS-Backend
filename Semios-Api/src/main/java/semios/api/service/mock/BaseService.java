package semios.api.service.mock;

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import javax.servlet.http.HttpServletRequest;

/**
 * @program: 通用接口
 * @description:
 * @author: gengshimiao
 * @create: 2022-12-20 10:27
 **/
public abstract class BaseService {

    protected HttpHeaders createHttpHeaders(HttpServletRequest request) {
        String address = "";

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Accept", "application/json");
        headers.add("Referer", "http://localhost");
        headers.add("X-Requested-With", "XMLHttpRequest");
        headers.add("address", address);
        return headers;
    }

    protected HttpHeaders createHttpHeaders(HttpServletRequest request, String address) {

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("Accept", "application/json");
        headers.add("Referer", "http://localhost");
        headers.add("X-Requested-With", "XMLHttpRequest");
        headers.add("address", address);
        return headers;
    }
}
