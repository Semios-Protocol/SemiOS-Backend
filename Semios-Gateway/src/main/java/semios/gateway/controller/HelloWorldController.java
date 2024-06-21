package semios.gateway.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.bind.annotation.*;
import semios.gateway.model.entity.HelloWorld;

/**
 * @description: asset
 * @author: xiangbin
 * @create: 2022-04-21 08:50
 **/
@Slf4j
@RestController
@RequestMapping(value = "/hello")
public class HelloWorldController {


    @GetMapping(value = "/world")
    public String helloWorld() {

        return "hello world";
    }

    @GetMapping(value = "/getById")
    public String getNameById(@RequestParam(required = false) Integer id) {
        HelloWorld helloWorld = new HelloWorld();
        if (helloWorld != null && StringUtils.isNotBlank(helloWorld.getName())) {
            return helloWorld.getName();
        }
        return "default";
    }

    @PostMapping(value = "/getByName")
    public String getBodyByname(@RequestParam(required = false) String name) {
        HelloWorld helloWorld = new HelloWorld();
        if (StringUtils.isBlank(name)) {
            return "hi";
        }
        if (name.equals("nihao")) {
            return "nihao";
        } else if (name.equals("hello")) {
            return "hello";
        } else {
            return "world";
        }
    }
}
