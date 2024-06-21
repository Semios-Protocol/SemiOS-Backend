package semios.api.controller;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.servlet.http.HttpServletRequest;

/**
 * 测试XSS攻击
 *
 * @description: RsaControllerTest
 * @author: xiangbin
 * @create: 2023-01-09 11:40
 * @ignore
 **/
@Controller
@RequestMapping("/test")
public class RsaControllerTest {


    private static final Logger log = LoggerFactory.getLogger(RsaControllerTest.class);


    @RequestMapping("/testXSS")
    @ResponseBody
    public String testXSS(HttpServletRequest request) {

		/*String queryString = request.getQueryString();
		log.info("【防御XSS】,客户端端请求参数是：{}", queryString);*/

        String name = request.getParameter("name");
        log.info("【防御XSS】,客户端端name是：{}", name);

        return "【防御XSS】,客户端端请求参数：" + name;
    }
}