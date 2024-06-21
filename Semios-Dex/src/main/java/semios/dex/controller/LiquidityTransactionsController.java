package semios.dex.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.common.PageDto;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.common.ResultList;
import semios.dex.model.entity.LiquidityTransaction;
import semios.dex.model.vo.req.TransactionReqVo;
import semios.dex.model.vo.res.TransactionResVo;
import semios.dex.service.ILiquidityTransactionService;

import java.util.ArrayList;
import java.util.List;

/**
 * dex 和burn 页面transactions内容展示
 *
 * @author xiangbin
 */
@RestController
@RequestMapping("/liquidity")
public class LiquidityTransactionsController extends BaseController {

    @Autowired
    private ILiquidityTransactionService liquidityTransactionService;

    /**
     * erc20的交易信息查询接口
     *
     * @return
     * @apiNote erc20的交易信息查询（每页10条数据）（返回数据集合dataList）
     */
    @PostMapping("/transaction")
    public ResultList<TransactionResVo> transactions(@RequestBody(required = true) TransactionReqVo transactionReqVo) {
        ResultList<TransactionResVo> result = new ResultList<>();
        if (!checkAddress(transactionReqVo.getErc20Address())) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        transactionReqVo.setErc20Address(formatAddress(transactionReqVo.getErc20Address()));
        IPage<LiquidityTransaction> transactionPage = liquidityTransactionService.getTradeList(transactionReqVo);
        List<LiquidityTransaction> liquidityTransactionList = transactionPage.getRecords();
        List<TransactionResVo> transactionResList = new ArrayList<TransactionResVo>();
        for (LiquidityTransaction liquidityTransaction : liquidityTransactionList) {
            TransactionResVo transactionResVo = TransactionResVo.transfer(liquidityTransaction);
            transactionResList.add(transactionResVo);
        }

        result.setDataList(transactionResList);
        PageDto page = new PageDto();
        page.setPageNo(transactionReqVo.getPageNo());
        page.setPageSize(transactionReqVo.getPageSize());
        page.setCount(transactionPage.getTotal());
        result.setPage(page);
        return result;
    }
}
