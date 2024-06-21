package semios.api.controller;

import com.alibaba.fastjson.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.HashMap;
import java.util.Map;

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
public class DaoAnalyticsControllerTestBase extends BaseSpringBootTest {

    @Autowired
    private DaoAnalyticsController daoAnalyticsController;

    private MockMvc mockMvc;

    @Before
    public void setup() {
        mockMvc = MockMvcBuilders.standaloneSetup(daoAnalyticsController).build();
    }

//    @Test
//    public void demo() throws Exception {
//        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.get("/order/add"))
//                .andExpect(MockMvcResultMatchers.status().isOk())
//                .andDo(MockMvcResultHandlers.print())
//                .andReturn();
//
//        logger.info(mvcResult.getResponse().getContentAsString());
//
//    }

    @Test
    public void togetherDaoMemberTest() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "103");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/togetherDao/member")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void detailV2Test() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "102");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/detail/v2")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
//                        .accept(MediaType.APPLICATION_JSON).param("daoId", "102"))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void togetherDaoListTest() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "103");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/togetherDao/list")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }


    @Test
    public void togetherDaoMakerTest() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "103");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/togetherDao/maker")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
//                        .accept(MediaType.APPLICATION_JSON).param("daoId", "103"))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void togetherDaoTokenTest() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "103");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/togetherDao/token")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void togetherDaoAmountTest() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "103");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/togetherDao/amount")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }

    @Test
    public void togetherDaoInfoTest() throws Exception {
        Map<String, String> map = new HashMap<>();
        map.put("daoId", "103");
        MvcResult mvcResult = mockMvc.perform(MockMvcRequestBuilders.post("/dao/analytics/togetherDao/info")
                        .content(JSONObject.toJSONString(map)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        logger.info(mvcResult.getResponse().getContentAsString());

    }

}



