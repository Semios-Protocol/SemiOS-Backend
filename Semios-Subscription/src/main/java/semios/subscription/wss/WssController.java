package semios.subscription.wss;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @description: WSS
 * @author: xiangbin
 * @create: 2022-04-07 16:51
 **/
@RestController
public class WssController {

    @GetMapping(value = "test")
    public String hello() {
        return "HELLO WORLD";
    }
}
