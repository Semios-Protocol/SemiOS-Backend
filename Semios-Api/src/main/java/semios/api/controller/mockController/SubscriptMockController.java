package semios.api.controller.mockController;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.request.SubscribeRequestDto;
import semios.api.utils.JacksonUtil;

import javax.servlet.http.HttpServletRequest;

/**
 * 订阅功能测试接口
 *
 * @ignore
 */
@Slf4j
@RestController
@RequestMapping("/event")
public class SubscriptMockController {

    @PostMapping(value = "/query/transaction/firstlog")
    public Result<String> ethGetTransactionFirstLog(@RequestParam(required = false) String netWork,
                                                    @RequestParam(required = false) String transactionHash,
                                                    HttpServletRequest request) {
        Result<String> result = new Result<>();
        log.info("ethGetTransactionFirstLog--mock-test,netWork:" + netWork);
        log.info("ethGetTransactionFirstLog--mock-test,transactionHash:" + transactionHash);
        result.setData("1");
        return result;
    }

    @PostMapping("/query/transaction")
    public Result<String> ethGetTransactionReceipt(@RequestParam(required = false) String netWork,
                                                   @RequestParam(required = false) String transactionHash) {
        Result<String> result = new Result<>();
        log.info("ethGetTransactionReceipt--mock-test,netWork:" + netWork);
        log.info("ethGetTransactionReceipt--mock-test,transactionHash:" + transactionHash);
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/subscribe")
    public Result<String> subscripe(@RequestBody(required = false) SubscribeRequestDto subscribe) {
        Result<String> result = new Result<>();
        log.info("subscripe--mock-test,subscribe:" + JacksonUtil.obj2json(subscribe));
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/subscribe/status")
    public Result<String> subscripeStatus(@RequestParam(required = false) String subId) {
        Result<String> result = new Result<>();
        log.info("subscripeStatus--mock-test,subId:" + subId);
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/call")
    public Result<String> infuraCall(@RequestBody(required = false) InfuraCallRequestDto infuraCallRequestDto) {
        Result<String> result = new Result<>();
        log.info("infuraCall--mock-test,infuraCallRequestDto:" + JacksonUtil.obj2json(infuraCallRequestDto));
        result.setData("0x0000000000000000000000000000000000000000000000000000000000000012");
        return result;
    }

    @PostMapping(value = "/subscribe/heigh")
    public Result<String> subscripeHeight(@RequestParam(required = false) String subId) {
        Result<String> result = new Result<>();
        log.info("subscripeHeight--mock-test,subId:" + subId);
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/eth/blockNumber")
    public Result<String> ethGetBlockNumber(@RequestParam(required = false) String netWork) {
        Result<String> result = new Result<>();
        log.info("ethGetBlockNumber--mock-test,netWork:" + netWork);
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/query/blockTime")
    public Result<String> queryBlockTime(@RequestParam(required = false) String netWork, @RequestParam(required = false) String blockNumber) {
        Result<String> result = new Result<>();
        log.info("queryBlockTime--mock-test,netWork:" + netWork);
        log.info("queryBlockTime--mock-test,blockNumber:" + blockNumber);
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/eth/getBalance")
    public Result<String> ethGetBalance(@RequestParam(required = false) String netWork,
                                        @RequestParam(required = false) String contractAddress) {
        Result<String> result = new Result<>();
        log.info("ethGetBalance--mock-test,netWork:" + netWork);
        log.info("ethGetBalance--mock-test,contractAddress:" + contractAddress);
        result.setData("1");
        return result;
    }

    @PostMapping(value = "/query/transaction/hash")
    public Result<String> ethGetTransactionByHash(@RequestParam(required = false) String netWork,
                                                  @RequestParam(required = false) String transactionHash) {
        Result<String> result = new Result<>();
        log.info("ethGetTransactionByHash--mock-test,netWork:" + netWork);
        log.info("ethGetTransactionByHash--mock-test,transactionHash:" + transactionHash);
        result.setData("1");
        return result;
    }

    /**
     * 通过地址查询eth_getCode方法，判断是用户地址还是合约地址
     *
     * @param netWork 网络
     * @param address 用户地址或者合约地址
     * @return Boolean true为用户地址， false为合约地址或者错误地址
     */
    public @PostMapping(value = "/eth/getCode/userAddress")
    Result<Boolean> ethGetCodeCheckUserAddress(@RequestParam(required = false) String netWork,
                                               @RequestParam(required = false) String address) {
        Result<Boolean> result = new Result<>();
        log.info("ethGetTransactionByHash--mock-test,netWork:" + netWork);
        log.info("ethGetTransactionByHash--mock-test,transactionHash:" + address);
        result.setData(true);
        return result;
    }

}
