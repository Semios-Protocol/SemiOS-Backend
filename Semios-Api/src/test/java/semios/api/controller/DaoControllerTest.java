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
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.req.UserProfilePageReqVo;
import semios.api.model.vo.res.DaoNameListVo;
import semios.api.model.vo.res.TogetherDaoListVo;
import semios.api.model.vo.res.WorkListVo;
import semios.api.service.mock.BaseService;

import javax.servlet.http.HttpServletRequest;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
class DaoControllerTest extends BaseService {

    String rootUrl = "http://127.0.0.1:9381/";

    @Autowired
    private HttpServletRequest request;

    private final  String address = "0x125d349A706f7fE6790ac73b1C32e000A6919b12";
    @Test
    public void daoCollections() {
        String url = rootUrl + "/dao/collections";
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        HttpHeaders headers = this.createHttpHeaders(request);
        HttpEntity<DaoSortedReqVo> httpEntity = new HttpEntity<>(daoSortedReqVo, headers);
        Result<TogetherDaoListVo> returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        // select by dao id
        daoSortedReqVo.setDaoId("1"); // dao id
        httpEntity = new HttpEntity<>(daoSortedReqVo, headers);
        returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());

        if (!returnVo.getDataList().isEmpty()){
            Integer getId = returnVo.getDataList().get(0).getDaoId();
            Assert.assertEquals("成功", 1L,getId.longValue());
        }
    }

    @Test
    public void unmintedWorks() {
        String url = rootUrl + "/dao/unmintedWorks";
        DaoSortedReqVo daoSortedReqVo = new DaoSortedReqVo();

        HttpHeaders headers = this.createHttpHeaders(request);
        HttpEntity<DaoSortedReqVo> httpEntity = new HttpEntity<>(daoSortedReqVo, headers);
        Result<WorkListVo> returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());


        // select by dao id
        daoSortedReqVo.setDaoId("1"); // dao id
        httpEntity = new HttpEntity<>(daoSortedReqVo, headers);
        returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
    }

    @Test
    public void myDao() {
        String url = rootUrl + "/dao/myDao";
        UserProfilePageReqVo userProfilePageReqVo = new UserProfilePageReqVo();
        userProfilePageReqVo.setPageSize(5L);
        userProfilePageReqVo.setPageNo(1L);

        HttpHeaders headers = this.createHttpHeaders(request,address);
        HttpEntity<UserProfilePageReqVo> httpEntity = new HttpEntity<>(userProfilePageReqVo, headers);
        Result<DaoNameListVo> returnVo = new RestTemplate().postForObject(url, httpEntity, Result.class);

        Assert.assertEquals("成功", ResultDesc.SUCCESS.getResultCode(),returnVo.getResultCode());
        Assert.assertEquals("成功", 5L,returnVo.getDataList().size());
    }

}