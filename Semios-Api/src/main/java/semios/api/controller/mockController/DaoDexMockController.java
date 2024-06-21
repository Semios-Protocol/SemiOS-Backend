package semios.api.controller.mockController;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.Result;
import semios.api.model.vo.req.Erc20LiquidityReqVo;
import semios.api.model.vo.req.LiquidityTransactionReqVo;
import semios.api.model.vo.req.UserLiquidityStatisticsVo;
import semios.api.utils.JacksonUtil;

/**
 * 订阅功能测试接口
 *
 * @ignore
 */
@Slf4j
@RestController
@RequestMapping("/sync")
public class DaoDexMockController {

    // 同步burn交易 返回更新条数
    @PostMapping("/transaction")
    public Result<Integer> dexTransactionAdd(@RequestBody(required = false) LiquidityTransactionReqVo liquidityTransaction) {
        Result<Integer> result = new Result<>();
        log.info("dexTransactionAdd--mock-test,liquidityTransaction:" + JacksonUtil.obj2json(liquidityTransaction));
        result.setData(0);
        return result;
    }

    // dao开始后同步到dex
    @PostMapping("/erc20")
    public Result<Integer> dexErc20Add(@RequestBody(required = false) Erc20LiquidityReqVo erc20LiquidityReqVo) {
        Result<Integer> result = new Result<>();
        log.info("dexErc20Add--mock-test,erc20LiquidityReqVo:" + JacksonUtil.obj2json(erc20LiquidityReqVo));
        result.setData(0);
        return result;
    }

    // 同步用户erc20的数量
    @PostMapping("/erc20Balance")
    public Result<Integer> syncErc20Balance(@RequestBody(required = false) UserLiquidityStatisticsVo userLiquidityStatisticsVo) {
        Result<Integer> result = new Result<>();
        log.info("syncErc20Balance--mock-test,userLiquidityStatisticsVo:" + JacksonUtil.obj2json(userLiquidityStatisticsVo));
        result.setData(0);
        return result;
    }

    @PostMapping("/erc20Owner")
    public Result<Integer> erc20Owner(@RequestBody(required = false) UserLiquidityStatisticsVo userLiquidityStatisticsVo) {
        Result<Integer> result = new Result<>();
        log.info("erc20Owner--mock-test,userLiquidityStatisticsVo:" + JacksonUtil.obj2json(userLiquidityStatisticsVo));
        result.setData(0);
        return result;
    }

}

