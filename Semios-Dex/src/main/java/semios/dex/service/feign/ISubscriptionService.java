package semios.dex.service.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.request.InfuraCallRequestDto;
import semios.dex.model.dto.request.SubscribeRequestDto;

@FeignClient(name = "subscription-client", url = "${subscription.service.url}")
public interface ISubscriptionService {


    //查询第一个log
    @PostMapping("/event/query/transaction/firstlog")
    Result<String> ethGetTransactionFirstLog(@RequestParam(required = false) String netWork, @RequestParam(required = false) String transactionHash);

    @PostMapping("/event/query/transaction")
    Result<String> ethGetTransactionReceipt(@RequestParam(required = false) String netWork, @RequestParam(required = false) String transactionHash);

    @PostMapping(value = "/event/subscribe")
    Result<String> subscripe(@RequestBody(required = false) SubscribeRequestDto subscribe);

    @PostMapping(value = "/event/subscribe/status")
    Result<String> subscripeStatus(@RequestParam(required = false) String subId);

    @PostMapping(value = "/event/call")
    Result<String> infuraCall(@RequestBody(required = false) InfuraCallRequestDto infuraCallRequestDto);

    @PostMapping(value = "/event/subscribe/heigh")
    Result<String> subscripeHeight(@RequestParam(required = false) String subId);

    @PostMapping(value = "/event/eth/blockNumber")
    Result<String> ethGetBlockNumber(@RequestParam(required = false) String netWork);


    @PostMapping(value = "/event/eth/getBalance")
    Result<String> ethGetBalance(@RequestParam(required = false) String netWork, @RequestParam(required = false) String contractAddress);

    @PostMapping(value = "/event/query/transaction/hash")
    Result<String> ethGetTransactionByHash(@RequestParam(required = false) String netWork, @RequestParam(required = false) String transactionHash);

    /**
     * 通过地址查询eth_getCode方法，判断是用户地址还是合约地址
     *
     * @param netWork 网络
     * @param address 用户地址或者合约地址
     * @return Boolean true为用户地址， false为合约地址或者错误地址
     */
    @PostMapping(value = "/event/eth/getCode/userAddress")
    Result<Boolean> ethGetCodeCheckUserAddress(@RequestParam(required = false) String netWork,
                                               @RequestParam(required = false) String address);


}
