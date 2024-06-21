package semios.api.controller;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import semios.api.service.common.CommonService;

/**
 * 单元测试说明，需要本地建数据库，然后倒入数据，
 * 1. 把ApplicationStartListener 的run方法内容注释掉
 * 2.  ProtoDaoApplication内的@EnableFeignClients注释掉
 * 3.  application.properties  改为spring.profiles.active=dev
 *
 * @description:
 * @author: xiangbin
 * @create: 2024-01-08 11:56
 **/
public class CommonServiceTest extends BaseSpringBootTest {

    @Autowired
    private CommonService commonService;


    @Test
    public void tokenHolders() throws Exception {

        String tokenAddress = "0xf49db90a899e124253e2b907edaed40a52b9fd78";
        int number = commonService.tokenHolders(tokenAddress);
        logger.info("[tokenHolders] return nums:{}", number);

    }


}



