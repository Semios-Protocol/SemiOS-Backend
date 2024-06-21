package semios.api.service.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import semios.api.model.dto.common.Result;
import semios.api.model.vo.req.Erc20LiquidityReqVo;
import semios.api.model.vo.req.LiquidityTransactionReqVo;
import semios.api.model.vo.req.UserLiquidityStatisticsVo;

@FeignClient(name = "protodao-dex-client", url = "${protodao.dex.service.url}")
public interface IProtoDaoDexService {

    // 同步burn交易 返回更新条数
    @PostMapping("/sync/transaction")
    Result<Integer> dexTransactionAdd(@RequestBody(required = false) LiquidityTransactionReqVo liquidityTransaction);

    // dao开始后同步到dex
    @PostMapping("/sync/erc20")
    Result<Integer> dexErc20Add(@RequestBody(required = false) Erc20LiquidityReqVo erc20LiquidityReqVo);

    // 同步用户erc20的数量
    @PostMapping("/sync/erc20Balance")
    Result<Integer>
    syncErc20Balance(@RequestBody(required = false) UserLiquidityStatisticsVo userLiquidityStatisticsVo);

    @PostMapping("/sync/erc20Owner")
    Result<Integer> erc20Owner(@RequestBody(required = false) UserLiquidityStatisticsVo userLiquidityStatisticsVo);

}
