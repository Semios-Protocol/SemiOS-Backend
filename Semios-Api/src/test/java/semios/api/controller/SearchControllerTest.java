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
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.vo.req.SearchReqVo;
import semios.api.model.vo.res.TogetherDaoListVo;
import semios.api.service.mock.BaseService;

import javax.servlet.http.HttpServletRequest;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
class SearchControllerTest extends BaseService {

    String rootUrl = "http://127.0.0.1:9381/";

    @Autowired
    private HttpServletRequest request;

    @Test
    public void searchDaoResult() {
        String url = rootUrl + "/search/daos";
        SearchReqVo searchReqVo = new SearchReqVo();
        searchReqVo.setSearchWord("zhy");
        searchReqVo.setNumber(1);

        HttpHeaders headers = this.createHttpHeaders(request);
        HttpEntity<SearchReqVo> httpEntity = new HttpEntity<>(searchReqVo, headers);
        Result<TogetherDaoListVo> returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

    }

    @Test
    public void searchWorkResult() {
        String url = rootUrl + "/search/works";
        SearchReqVo searchReqVo = new SearchReqVo();
        searchReqVo.setSearchWord("zhy");
        searchReqVo.setNumber(1);

        HttpHeaders headers = this.createHttpHeaders(request);
        HttpEntity<SearchReqVo> httpEntity = new HttpEntity<>(searchReqVo, headers);
        Result<TogetherDaoListVo> returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }
}