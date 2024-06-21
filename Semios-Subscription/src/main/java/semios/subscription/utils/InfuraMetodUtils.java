package semios.subscription.utils;

import com.sun.javafx.binding.StringFormatter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;
import semios.subscription.listener.EventListener;
import semios.subscription.model.constant.InfuraMethodConstant;
import semios.subscription.model.dto.CallParams;
import semios.subscription.model.dto.request.InfuraParamsForFilterDto;
import semios.subscription.model.dto.request.InfuraRequestDto;
import semios.subscription.model.dto.response.InfuraResponseDto;
import semios.subscription.model.dto.response.InfuraResponseObjectDto;
import semios.subscription.model.dto.response.InfuraResponseTransactionListDto;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @description: utils
 * @author: xiangbin
 * @create: 2022-04-13 15:44
 **/
@Slf4j
@Configuration
public class InfuraMetodUtils {


    private static String infuraUrl;

    public static String ethBlockNumber(String network) {
//        log.info("[InfuraMetodUtils-ethBlockNumber]network:{}", network);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_blockNumber, null, String.class);
        InfuraResponseDto infuraResponseDto = JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
//        log.info("[InfuraMetodUtils-ethBlockNumber]response:{}", responseEntity.getBody());
        if (infuraResponseDto == null || StringUtils.isBlank(infuraResponseDto.getResult())) {
            return null;
        }
        return infuraResponseDto.getResult();
    }

    public static InfuraResponseDto ethGetBalance(String network, String contractAddress) {

        List<Object> params = new ArrayList<>();
        params.add(contractAddress);
        params.add("latest");

//        log.info("[InfuraMetodUtils-ethCall]network:{}infuraParam:{}", network, JacksonUtil.obj2json(params));
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getBalance, params, String.class);
//        log.info("[InfuraMetodUtils-ethCall]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
    }

    public static InfuraResponseDto ethNewFilter(String network, InfuraParamsForFilterDto infuraParamsForFilterDto) {

//        log.info("[InfuraMetodUtils-ethNewFilter]network:{} infuraParamsForFilterDto:{}", network, JacksonUtil.obj2json(infuraParamsForFilterDto));
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_newFilter, infuraParamsForFilterDto, String.class);
//        log.info("[InfuraMetodUtils-ethNewFilter]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
    }

    public static InfuraResponseTransactionListDto ethGetFilterChanges(String network, String fileterId) {
//        log.info("[InfuraMetodUtils-ethGetFilterChanges]network:{}", network);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getFilterChanges, fileterId, String.class);
//        log.info("[InfuraMetodUtils-ethGetFilterChanges]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseTransactionListDto.class);
    }

    public static InfuraResponseTransactionListDto ethGetFilterLogs(String network, String fileterId) {
//        log.info("[InfuraMetodUtils-ethGetFilterLogs]network:{}", network);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getFilterLogs, fileterId, String.class);
//        log.info("[InfuraMetodUtils-ethGetFilterLogs]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseTransactionListDto.class);
    }

    public static InfuraResponseDto ethNewBlockFilter(String network) {

//        log.info("[InfuraMetodUtils-ethNewBlockFilter]network:{}", network);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_newBlockFilter, null, String.class);
//        log.info("[InfuraMetodUtils-ethNewBlockFilter]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
    }

    public static InfuraResponseDto ethCall(String network, CallParams callParams, String blockNumber) {

        List<Object> params = new ArrayList<>();
        params.add(callParams);
        if (StringUtils.isNotBlank(blockNumber)) {
            params.add(blockNumber);
        } else {
            params.add("latest");
        }
//        log.info("[InfuraMetodUtils-ethCall]network:{}infuraParam:{}", network, JacksonUtil.obj2json(params));
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_call, params, String.class);
//        log.info("[InfuraMetodUtils-ethCall]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
    }

    public static InfuraResponseDto ethGasPrice(String network) {

//        log.info("[InfuraMetodUtils-ethGasPrice]network:{}", network);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_gasPrice, null, String.class);
//        log.info("[InfuraMetodUtils-ethGasPrice]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
    }

    /**
     * @param network
     * @param blockNumber 为0x加16进制的数
     * @return
     */
    public static InfuraResponseObjectDto ethGetBlockByNumber(String network, String blockNumber) {

        List<Object> params = new ArrayList<>();
        params.add(blockNumber);
        params.add(false);
//        log.info("[InfuraMetodUtils-ethGetBlockByNumber]network:{}infuraParam:{}", network, JacksonUtil.obj2json(params));
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getBlockByNumber, params, String.class);
//        log.info("[InfuraMetodUtils-ethGetBlockByNumber]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseObjectDto.class);
    }

    public static InfuraResponseObjectDto ethGetTransactionReceipt(String network, String transactionHash) {

//        log.info("[InfuraMetodUtils-ethGetTransactionReceipt]network:{}transactionHash:{}", network, transactionHash);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getTransactionReceipt, transactionHash, String.class);
//        log.info("[InfuraMetodUtils-ethGetTransactionReceipt]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseObjectDto.class);
    }

    public static InfuraResponseObjectDto ethGetTransactionByHash(String network, String transactionHash) {

//        log.info("[InfuraMetodUtils-ethGetTransactionReceipt]network:{}transactionHash:{}", network, transactionHash);
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getTransactionByHash, transactionHash, String.class);
//        log.info("[InfuraMetodUtils-ethGetTransactionReceipt]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseObjectDto.class);
    }

    /**
     * Returns the compiled smart contract code 返回0x为用户地址
     *
     * @param network 网络
     * @param address 用户地址或者合约地址
     * @return
     */
    public static InfuraResponseDto ethGetCode(String network, String address) {

        List<Object> params = new ArrayList<>();
        params.add(address);
        params.add("latest");
//        log.info("[InfuraMetodUtils-ethGetBlockByNumber]network:{}infuraParam:{}", network, JacksonUtil.obj2json(params));
        ResponseEntity<String> responseEntity = postForEntity(network, InfuraMethodConstant.eth_getCode, params, String.class);
//        log.info("[InfuraMetodUtils-ethGetBlockByNumber]response:{}", responseEntity.getBody());
        return JacksonUtil.json2pojo(responseEntity.getBody(), InfuraResponseDto.class);
    }

    //==================private==================//
    private static <T> ResponseEntity<T> postForEntity(String network, String method, Object param, Class<T> responseType) {

        String requestUrl = StringFormatter.format(infuraUrl, network, EventListener.infuraProjectId).getValue();

        InfuraRequestDto infuraParam = new InfuraRequestDto();
        infuraParam.setMethod(method);
        if (param != null) {
            if (param instanceof List) {
                infuraParam.setParams((List) param);
            } else {
                infuraParam.setParams(Collections.singletonList(param));
            }
        }
//        log.info("[InfuraMetodUtils-postForEntity]:infuraParam{}", JacksonUtil.obj2json(infuraParam));
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Type", "application/json");
        HttpEntity<InfuraRequestDto> r = new HttpEntity<>(infuraParam, headers);
//        RestTemplate restTemplate = SpringBeanUtil.getBean(RestTemplate.class);
//        if (restTemplate == null) {
//            restTemplate = new RestTemplate();
//        }
        RestTemplate restTemplate = createCustomRestTemplate();
        return restTemplate.postForEntity(requestUrl, r, responseType);
    }

    private static RestTemplate createCustomRestTemplate() {
        SimpleClientHttpRequestFactory factory = new SimpleClientHttpRequestFactory();
        factory.setConnectTimeout((int) Duration.ofSeconds(20).toMillis()); // 连接超时
        factory.setReadTimeout((int) Duration.ofSeconds(20).toMillis());    // 读取超时
        return new RestTemplate(factory);
    }

    public static void main(String[] args) {
//        String requestUrl = StringFormatter.format(INFUR_URL, "127.0.0.1", "123456").getValue();
//        System.out.println(requestUrl);

//        System.out.println(Integer.parseInt("12193959", 16));
//        InfuraResponseDto infuraResponseDto = JacksonUtil.json2pojo("{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"0xba16b0\"}", InfuraResponseDto.class);
//        System.out.println(infuraResponseDto.getResult());

//        System.out.println(Integer.parseInt(CommonUtil.removeHexPrefixIfExists("ba3503"), 16));
        InfuraRequestDto infuraParam = new InfuraRequestDto();
        List<Object> params = new ArrayList<>();
        params.add("{\"from\": \"0xb60e8dd61c5d32be8058bb8eb970870f07233155\",\"to\": \"0xd46e8dd67c5d32be8058bb8eb970870f07244567\",\"gas\": \"0x76c0\",\"gasPrice\": \"0x9184e72a000\",\"value\": \"0x9184e72a\",\"data\": \"0xd46e8dd67c5d32be8d46e8dd67c5d32be8058bb8eb970870f072445675058bb8eb970870f072445675\"}");
        params.add(false);
        if (params instanceof List) {
            infuraParam.setParams(params);
        }
        String requestUrl = StringFormatter.format(infuraUrl, "mainnet", "1cfe15d1bb424b1ab863a1e045cfb69a").getValue();
        ResponseEntity<String> responseEntity = postForEntity(requestUrl, InfuraMethodConstant.eth_call, params, String.class);
        System.out.println(responseEntity.getBody());
        System.out.println(JacksonUtil.obj2json(infuraParam));

    }

    @Value("${infura.url}")
    public void setInfuraUrl(String infuraUrl) {
        InfuraMetodUtils.infuraUrl = infuraUrl;
    }
}
