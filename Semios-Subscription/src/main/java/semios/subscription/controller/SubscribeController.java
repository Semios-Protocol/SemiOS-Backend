package semios.subscription.controller;


import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;
import semios.subscription.model.dto.CallParams;
import semios.subscription.model.dto.NoticeSubValueDto;
import semios.subscription.model.dto.common.Result;
import semios.subscription.model.dto.common.ResultDesc;
import semios.subscription.model.dto.request.InfuraCallRequestDto;
import semios.subscription.model.dto.request.InfuraParamsForFilterDto;
import semios.subscription.model.dto.request.Subscribe;
import semios.subscription.model.dto.response.InfuraResponseDto;
import semios.subscription.model.dto.response.InfuraResponseObjectDto;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.model.entity.Subscriber;
import semios.subscription.model.enums.NetWorkEnum;
import semios.subscription.model.enums.NoticeTypeEnum;
import semios.subscription.model.enums.SubStatusEnum;
import semios.subscription.service.IBlockHeightService;
import semios.subscription.service.ISubscriberService;
import semios.subscription.utils.CommonUtil;
import semios.subscription.utils.InfuraMetodUtils;
import semios.subscription.utils.JacksonUtil;
import semios.subscription.utils.SpringBeanUtil;

import java.util.List;
import java.util.Map;


/**
 * <p>
 * 订阅记录表 前端控制器
 * </p>
 *
 * @author xiangbin
 */
@Slf4j
@RestController
@RequestMapping("/event")
public class SubscribeController {


    @Autowired
    private ISubscriberService subscriberService;

    @Autowired
    private IBlockHeightService blockHeightService;

    @PostMapping(value = "/subscribe")
    public Result<String> subscripe(@RequestBody(required = false) Subscribe subscribe) {
        log.info("[event-subscribe]param:{}", JacksonUtil.obj2json(subscribe));
        Result<String> result = new Result<>();
        if (subscribe == null || StringUtils.isAnyBlank(subscribe.getNetwork(), subscribe.getAddress(), subscribe.getNoticeUrl())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        NetWorkEnum netWorkEnum = NetWorkEnum.getByName(subscribe.getNetwork());
        if (netWorkEnum == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("network is not available");
            return result;
        }
        subscribe.setNetwork(netWorkEnum.getValue());
        Result<String> subscriberResult;
        Subscriber subscriber = new Subscriber();

        if (subscribe.getNoticeType() == null || subscribe.getNoticeType().equals(NoticeTypeEnum.TRAN.getType()) || subscribe.getNoticeType().equals(NoticeTypeEnum.BATCH_TRAN.getType())) {
            //订阅transaction通知
            subscriberResult = subscribeForTran(subscribe, subscriber);
        } else {
            //订阅数值查询通知
            subscriberResult = subscribeForNum(subscribe, subscriber);
        }
        if (subscriberResult.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            return subscriberResult;
        }

        subscriber.setSubStatus(SubStatusEnum.OPEN.getStatus());
        if (subscribe.getIntervalPeriod() != null) {
            subscriber.setIntervalTime(subscribe.getIntervalPeriod());
        } else {
            subscriber.setIntervalTime(10);
        }

        List<Subscriber> subscribers = subscriberService.findSubByAppNameAnd(subscriber);
        if (subscribers != null && subscribers.size() > 0) {
            log.info("[event-subscribe]param:{} return subscribers id:{}", JacksonUtil.obj2json(subscribe), subscribers.get(0).getId());
            result.setData(subscribers.get(0).getId().toString());
            return result;
        }

        int i = subscriberService.saveSub(subscriber);
        if (i <= 0) {
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("订阅保存异常，请稍后再试！");
            return result;
        }

        log.info("[event-subscribe]param:{} return  id:{}", JacksonUtil.obj2json(subscribe), subscriber.getId());
        result.setData(subscriber.getId().toString());

        return result;
    }


    @PostMapping(value = "/subscribe/status")
    public Result<String> subscripeStatus(@RequestParam(required = false) String subId) {
        Result<String> result = new Result<>();
        if (StringUtils.isBlank(subId)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("subId is null");
            return result;
        }
        Subscriber subscriber = subscriberService.getById(subId);
        if (subscriber == null) {
            result.setData(String.valueOf(SubStatusEnum.CLOSE.getStatus()));
            return result;
        }
        result.setData(String.valueOf(subscriber.getSubStatus()));

        return result;
    }

    @PostMapping(value = "/subscribe/heigh")
    public Result<String> subscripeHeight(@RequestParam(required = false) String subId) {
        Result<String> result = new Result<>();
        if (StringUtils.isBlank(subId)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        Subscriber subscriber = subscriberService.getById(subId);
        if (subscriber == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        BlockHeight blockHeight = blockHeightService.getBySubId(String.valueOf(subscriber.getId()));
        if (blockHeight == null || StringUtils.isBlank(blockHeight.getToBlock())) {
            return result;
        }
        result.setData(String.valueOf(blockHeight.getToBlock()));

        return result;
    }


    @PostMapping(value = "/call")
    public Result<String> infuraCall(@RequestBody(required = false) InfuraCallRequestDto infuraCallRequestDto) {
        Result<String> result = new Result<>();
        log.info("[ethCall-param] infuraCallRequestDto:{}", JacksonUtil.obj2json(infuraCallRequestDto));
        if (infuraCallRequestDto == null || StringUtils.isBlank(infuraCallRequestDto.getNetWork())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        NetWorkEnum netWorkEnum = NetWorkEnum.getByName(infuraCallRequestDto.getNetWork());
        if (netWorkEnum == null) {
            log.info("[ethCall-param] netWorkEnum is null infuraCallRequestDto:{}", JacksonUtil.obj2json(infuraCallRequestDto));
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("network is not available");
            return result;
        }
        if (StringUtils.isBlank(infuraCallRequestDto.getBlockNumber())) {
            infuraCallRequestDto.setBlockNumber("latest");
        }
        CallParams callParams = CallParams.builder().from(CommonUtil.addHexPrefixIfNotExist(infuraCallRequestDto.getFrom())).to(CommonUtil.addHexPrefixIfNotExist(infuraCallRequestDto.getTo()))
                .gas(infuraCallRequestDto.getGas()).gasPrice(infuraCallRequestDto.getGasPrice()).value(infuraCallRequestDto.getValue()).data(CommonUtil.addHexPrefixIfNotExist(infuraCallRequestDto.getData())).build();
        log.info("[ethCall-result] infuraCallRequestDto:{}, callParams:{}", JacksonUtil.obj2json(infuraCallRequestDto), JacksonUtil.obj2json(callParams));
        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethCall(infuraCallRequestDto.getNetWork(), callParams, infuraCallRequestDto.getBlockNumber());
        if (infuraResponseDto == null || StringUtils.isBlank(infuraResponseDto.getResult())) {
            if (infuraResponseDto != null && infuraResponseDto.getError() != null && StringUtils.isNotBlank(infuraResponseDto.getError().getMessage()) && infuraResponseDto.getError().getMessage().contains("burn amount exceeds balance")) {
                log.warn("[ethCall-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
            } else {
                log.error("[ethCall-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
            }
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc(infuraResponseDto.getError().getMessage());
            return result;
        }
        result.setData(CommonUtil.removeHexPrefixIfExists(infuraResponseDto.getResult()));
        return result;
    }

    @PostMapping(value = "/eth/gasPrice")
    public Result<String> ethGasPrice(@RequestParam(required = false) String netWork) {
        log.info("[ethGasPrice] netWork:{}", netWork);
        Result<String> result = new Result<>();
        if (StringUtils.isBlank(netWork)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethGasPrice(netWork);
        if (StringUtils.isBlank(infuraResponseDto.getResult())) {
            log.error("[ethGasPrice-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc(infuraResponseDto.getError().getMessage());
            return result;
        }
        result.setData(infuraResponseDto.getResult());

        return result;
    }

    @PostMapping(value = "/query/transaction")
    public Result<String> ethGetTransactionReceipt(@RequestParam(required = false) String netWork, @RequestParam(required = false) String transactionHash) {
        log.info("[ethGetTransactionReceipt] netWork:{}transactionHash:{}", netWork, transactionHash);
        Result<String> result = new Result<>();
        if (StringUtils.isAnyBlank(netWork, transactionHash)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        InfuraResponseObjectDto infuraResponseDto = InfuraMetodUtils.ethGetTransactionReceipt(netWork, transactionHash);
        if (infuraResponseDto.getResult() == null) {
            log.error("[ethGetTransactionReceipt-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc(infuraResponseDto.getError().getMessage());
            return result;
        }
        result.setData(JacksonUtil.obj2json(infuraResponseDto.getResult()));
        return result;
    }

    @PostMapping(value = "/query/transaction/hash")
    public Result<String> ethGetTransactionByHash(@RequestParam(required = false) String netWork, @RequestParam(required = false) String transactionHash) {
        log.info("[ethGetTransactionByHash] netWork:{}transactionHash:{}", netWork, transactionHash);
        Result<String> result = new Result<>();
        if (StringUtils.isAnyBlank(netWork, transactionHash)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        InfuraResponseObjectDto infuraResponseDto = InfuraMetodUtils.ethGetTransactionByHash(netWork, transactionHash);
        if (infuraResponseDto.getResult() == null) {
            log.error("[ethGetTransactionByHash-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc(infuraResponseDto.getError().getMessage());
            return result;
        }
        result.setData(JacksonUtil.obj2json(infuraResponseDto.getResult()));
        return result;
    }

    @PostMapping(value = "/query/transaction/firstlog")
    public Result<String> ethGetTransactionFirstLog(@RequestParam(required = false) String netWork, @RequestParam(required = false) String transactionHash) {
        log.info("[ethGetTransactionFirstLog] netWork:{} transactionHash:{}", netWork, transactionHash);
        Result<String> result = new Result<>();
        if (StringUtils.isAnyBlank(netWork, transactionHash)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        try {
            InfuraResponseObjectDto infuraResponseDto = InfuraMetodUtils.ethGetTransactionReceipt(netWork, transactionHash);
            if (infuraResponseDto.getResult() == null) {
                log.error("[ethGetTransactionFirstLog-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc(infuraResponseDto.getError().getMessage());
                return result;
            }
            Map<String, Object> objectMap = JacksonUtil.json2map(JacksonUtil.obj2json(infuraResponseDto.getResult()));
            if (objectMap == null) {
                log.error("[ethGetTransactionFirstLog-result] objectMap is null infuraResponseDto:{}", JacksonUtil.obj2json(infuraResponseDto));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc(ResultDesc.FAIL.getResultDesc());
                return result;
            }
            List<Object> objects = JacksonUtil.json2list(JacksonUtil.obj2json(objectMap.get("logs")), Object.class);
            result.setData(JacksonUtil.obj2json(objects.get(0)));

        } catch (Exception e) {
            log.error("[ethGetTransactionFirstLog] throw Exception e:", e);
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc(ResultDesc.ERROR.getResultDesc());
        }
        return result;
    }

    @PostMapping(value = "/query/blockTime")
    public Result<String> queryBlockTime(@RequestParam(required = false) String netWork, @RequestParam(required = false) String blockNumber) {
        //查询区块发生的时间
        Result<String> result = new Result<>();
        if (StringUtils.isAnyBlank(netWork, blockNumber)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        String timestamp = null;
        InfuraResponseObjectDto infuraResponseDto = InfuraMetodUtils.ethGetBlockByNumber(netWork, blockNumber);
        if (infuraResponseDto.getResult() != null) {
            Map<String, Object> map = JacksonUtil.json2map(JacksonUtil.obj2json(infuraResponseDto.getResult()));
            if (map == null) {
                result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
                result.setResultDesc(ResultDesc.NOT_FOUND_ERROR.getResultDesc());
                return result;
            }
            timestamp = (String) map.get("timestamp");
        }
        if (StringUtils.isBlank(timestamp)) {
            log.error("[query-blockTime] no timestamp block:{}", blockNumber);
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.NOT_FOUND_ERROR.getResultDesc());
            return result;
        }
        timestamp = Integer.parseInt(CommonUtil.removeHexPrefixIfExists(timestamp), 16) + "";
        result.setData(timestamp);
        return result;
    }


    @PostMapping(value = "/eth/blockNumber")
    public Result<String> ethGetBlockNumber(@RequestParam(required = false) String netWork) {
        log.info("[ethGetBlockNumber] netWork:{}", netWork);
        Result<String> result = new Result<>();
        if (StringUtils.isBlank(netWork)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        String blockNumber = InfuraMetodUtils.ethBlockNumber(netWork);
        log.info("[ethGetBlockNumber]netWork:{} blockNumber is {}", netWork, blockNumber);
        if (StringUtils.isBlank(blockNumber)) {
            log.error("[ethGetBlockNumber-result] blockNumber is null");
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc(ResultDesc.FAIL.getResultDesc());
            return result;
        }
        result.setData(blockNumber);
        return result;
    }


    @PostMapping(value = "/eth/getBalance")
    public Result<String> ethGetBalance(@RequestParam(required = false) String netWork, @RequestParam(required = false) String contractAddress) {
        log.info("[ethGetBalance] netWork:{} contractAddress:{}", netWork, contractAddress);
        Result<String> result = new Result<>();
        if (StringUtils.isAnyBlank(netWork, contractAddress)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethGetBalance(netWork, contractAddress);
        log.info("[ethGetBalance]netWork:{} infuraResponseDto is {}", netWork, JacksonUtil.obj2json(infuraResponseDto));
        if (infuraResponseDto == null || StringUtils.isBlank(infuraResponseDto.getResult())) {
            log.error("[ethGetBalance] infura error infuraResponseDto:{}", JacksonUtil.obj2json(infuraResponseDto));
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("请求infura异常，请检查参数后重试!");
            return result;
        }
        result.setData(infuraResponseDto.getResult());
        return result;
    }

    /**
     * 通过地址查询eth_getCode方法，判断是用户地址还是合约地址
     *
     * @param netWork 网络
     * @param address 用户地址或者合约地址
     * @return
     */
    @PostMapping(value = "/eth/getCode/userAddress")
    public Result<Boolean> ethGetCodeCheckUserAddress(@RequestParam(required = false) String netWork, @RequestParam(required = false) String address) {
        log.info("[ethGetCode] netWork:{} address:{}", netWork, address);
        Result<Boolean> result = new Result<>();
        result.setData(false);
        if (StringUtils.isAnyBlank(netWork, address)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            return result;
        }
        InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethGetCode(netWork, address);
        log.info("[ethGetCode]netWork:{} infuraResponseDto is {}", netWork, JacksonUtil.obj2json(infuraResponseDto));
        if (infuraResponseDto == null || StringUtils.isBlank(infuraResponseDto.getResult())) {
            log.error("[ethGetCode] infura error infuraResponseDto:{}", JacksonUtil.obj2json(infuraResponseDto));
            result.setResultCode(ResultDesc.FAIL.getResultCode());
            result.setResultDesc("请求infura异常，请检查参数后重试!");
            return result;
        }
        result.setData(infuraResponseDto.getResult().equals("0x"));
        return result;
    }

    //===================private=============//
    private Result<String> subscribeForTran(Subscribe subscribe, Subscriber subscriber) {

        Result<String> result = new Result<>();

        if (StringUtils.isNotBlank(subscribe.getFromBlock())) {
            if (!(CommonUtil.sixteen(CommonUtil.removeHexPrefixIfExists(subscribe.getFromBlock())) || subscribe.getFromBlock().equals("latest"))) {
                result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
                result.setResultDesc("fromBlock is not available");
                return result;
            }
        }

        try {


            BeanUtils.copyProperties(subscribe, subscriber);
            subscriber.setAddress(subscribe.getAddress());
            subscriber.setTopics(JacksonUtil.obj2json(subscribe.getTopics()));
            subscriber.setNetwork(subscribe.getNetwork());
            if (subscribe.getTopics() != null && subscribe.getTopics().size() > 0) {
                subscriber.setTopics(JacksonUtil.obj2json(subscribe.getTopics()));
            }

            //调用infura订阅一次
            String blockNo = InfuraMetodUtils.ethBlockNumber(subscribe.getNetwork());
            if (StringUtils.isBlank(blockNo)) {
                log.error("[event-subscribe]获取当前块为空param:{}", JacksonUtil.obj2json(subscribe));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("请求infura网络异常，请稍后再试!");
                return result;
            }
            if (StringUtils.isBlank(subscriber.getFromBlock())) {
                subscriber.setFromBlock(blockNo);
            } else {
                if (!subscriber.getFromBlock().equalsIgnoreCase("latest")) {
                    subscribe.setFromBlock(CommonUtil.addHexPrefixIfNotExist(subscribe.getFromBlock()));
                    subscriber.setFromBlock(CommonUtil.addHexPrefixIfNotExist(subscribe.getFromBlock()));
                    //判断是否大于当前区块高度 如果大于则使用获取到的当前块高度
                    if (Integer.parseInt(CommonUtil.removeHexPrefixIfExists(subscribe.getFromBlock()), 16) > Integer.parseInt(CommonUtil.removeHexPrefixIfExists(blockNo), 16)) {
                        log.info("[event-subscribe]订阅高度大于当前高度:{}", JacksonUtil.obj2json(subscribe));
                        subscribe.setFromBlock(CommonUtil.removeHexPrefixIfExists(blockNo));
                        subscriber.setFromBlock(CommonUtil.removeHexPrefixIfExists(blockNo));
                    }
                }
            }
            InfuraParamsForFilterDto infuraParamsForFilterDto = new InfuraParamsForFilterDto();
            infuraParamsForFilterDto.setFromBlock(subscriber.getFromBlock());
            infuraParamsForFilterDto.setAddress(CommonUtil.addHexPrefixIfNotExist(subscriber.getAddress()));
            infuraParamsForFilterDto.setTopics(subscribe.getTopics());
            InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethNewFilter(subscriber.getNetwork(), infuraParamsForFilterDto);

            if (infuraResponseDto == null || StringUtils.isBlank(infuraResponseDto.getResult())) {
                log.error("[event-subscribe] infura error subscriber:{},infuraResponseDto:{}", JacksonUtil.obj2json(subscriber), JacksonUtil.obj2json(infuraResponseDto));
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("请求infura异常，请检查参数后重试!");
                return result;
            }

            // 调用回调接口 判断noticeUrl是否可用
            HttpHeaders headers = new HttpHeaders();
            headers.add("Content-Type", "application/json");
            HttpEntity<String> r = new HttpEntity<>("{\"result\":[]}", headers);
            log.info("[event-subscribe]noticeUrl:{}", subscribe.getNoticeUrl());
            RestTemplate restTemplate = SpringBeanUtil.getBean(RestTemplate.class);
            if (restTemplate == null) {
                restTemplate = new RestTemplate();
            }
            ResponseEntity<String> responseEntity = restTemplate.postForEntity(subscribe.getNoticeUrl(), r, String.class);
            log.info("[event-subscribe]noticeUrl:{},response:{}", subscribe.getNoticeUrl(), responseEntity.getBody());
            if (!"SUCCESS".equalsIgnoreCase(responseEntity.getBody())) {
                result.setResultCode(ResultDesc.ERROR.getResultCode());
                result.setResultDesc("请求地址不可用！");
            }
        } catch (Exception e) {
            log.error("[event-subscribe]Exception param:{}e:", JacksonUtil.obj2json(subscribe), e);
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("订阅异常，请稍后再试！");
        }

        return result;
    }


    private Result<String> subscribeForNum(Subscribe subscribe, Subscriber subscriber) {

        Result<String> result = new Result<>();

        try {

            BeanUtils.copyProperties(subscribe, subscriber);
            subscriber.setAddress(subscribe.getAddress());
            subscriber.setTopics(JacksonUtil.obj2json(subscribe.getTopics()));
            subscriber.setNetwork(subscribe.getNetwork());

            if (subscribe.getTopics() == null || subscribe.getTopics().size() == 0) {
                result.setResultCode(ResultDesc.FAIL.getResultCode());
                result.setResultDesc("topics is must");
                return result;
            }
            if (subscribe.getTopics() != null && subscribe.getTopics().size() > 0) {
                subscriber.setTopics(JacksonUtil.obj2json(subscribe.getTopics()));
            }

            //查询合约方法一次
            for (String topic : subscribe.getTopics()) {
                CallParams callParams = CallParams.builder().to(subscribe.getAddress()).data(CommonUtil.addHexPrefixIfNotExist(topic)).build();
                log.info("[subscribeForNum-ethCall-result] netWork:{}, blockNumber:{}, callParams:{}", subscribe.getNetwork(), subscribe.getFromBlock(), JacksonUtil.obj2json(callParams));
                InfuraResponseDto infuraResponseDto = InfuraMetodUtils.ethCall(subscribe.getNetwork(), callParams, subscribe.getFromBlock());
                if (StringUtils.isBlank(infuraResponseDto.getResult())) {
                    log.error("[subscribeForNum-ethCall-result] is fail error:{}", JacksonUtil.obj2json(infuraResponseDto));
                    result.setResultCode(ResultDesc.FAIL.getResultCode());
                    result.setResultDesc(infuraResponseDto.getError().getMessage());
                    return result;
                }

                // 调用回调接口 判断noticeUrl是否可用
                HttpHeaders headers = new HttpHeaders();
                headers.add("Content-Type", "application/json");
                NoticeSubValueDto noticeSubValueDto = new NoticeSubValueDto();
                noticeSubValueDto.setType(CommonUtil.removeHexPrefixIfExists(topic));
                noticeSubValueDto.setValue(CommonUtil.hexToTenString(infuraResponseDto.getResult()));
                HttpEntity<String> r = new HttpEntity<>(JacksonUtil.obj2json(noticeSubValueDto), headers);
                log.info("[subscribeForNum-event-subscribe]noticeUrl:{}", subscribe.getNoticeUrl());
                RestTemplate restTemplate = SpringBeanUtil.getBean(RestTemplate.class);
                if (restTemplate == null) {
                    restTemplate = new RestTemplate();
                }
                ResponseEntity<String> responseEntity = restTemplate.postForEntity(subscribe.getNoticeUrl(), r, String.class);
                log.info("[subscribeForNum-event-subscribe]noticeUrl:{},response:{}", subscribe.getNoticeUrl(), responseEntity.getBody());
                if (!"SUCCESS".equalsIgnoreCase(responseEntity.getBody())) {
                    result.setResultCode(ResultDesc.ERROR.getResultCode());
                    result.setResultDesc("请求地址不可用！");
                    return result;
                }
            }
        } catch (Exception e) {
            log.error("[subscribeForNum-event-subscribe]Exception param:{}e:", JacksonUtil.obj2json(subscribe), e);
            result.setResultCode(ResultDesc.ERROR.getResultCode());
            result.setResultDesc("订阅异常，请稍后再试！");
        }
        return result;
    }


}
