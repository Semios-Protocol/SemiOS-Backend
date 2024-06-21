package semios.api.controller;

import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.context.web.WebAppConfiguration;

/**
 * @description:
 * @author: xiangbin
 * @create: 2024-01-08 11:53
 **/
@RunWith(SpringRunner.class)
@SpringBootTest
@WebAppConfiguration
public class BaseSpringBootTest {

    protected Logger logger = LoggerFactory.getLogger(BaseSpringBootTest.class);

    @Before
    public void init() {
        logger.info("开始测试...");
    }

    @After
    public void after() {
        logger.info("测试结束...");
    }

}
