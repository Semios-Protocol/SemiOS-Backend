package semios.api.controller;

import org.junit.Assert;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import semios.api.SemiosApiApplication;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.res.WorkListVo;
import semios.api.service.mock.BaseService;

import javax.servlet.http.HttpServletRequest;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
class ProtodDaoControllerTest extends BaseService {

    String rootUrl = "http://127.0.0.1:9381/";

    @Autowired
    private HttpServletRequest request;

    private final  String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";
    @Test
    void protodaoRelated() {
        String url = rootUrl + "/protodao/related";
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        HttpHeaders headers = this.createHttpHeaders(request,address);
        HttpEntity<DaoSortedReqVo> httpEntity = new HttpEntity<>(daoSortedReqVo, headers);
        ResultList<WorkListVo> returnVo = new RestTemplate().postForObject(url, httpEntity, ResultList.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        // select by dao id
        daoSortedReqVo.setDaoId("1"); // dao id
        httpEntity = new HttpEntity<>(daoSortedReqVo, headers);
        returnVo = new RestTemplate().postForObject(url, httpEntity, ResultList.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }
}