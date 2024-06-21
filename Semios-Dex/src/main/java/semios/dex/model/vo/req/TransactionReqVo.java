package semios.dex.model.vo.req;

import lombok.Data;
import lombok.EqualsAndHashCode;
import semios.dex.model.vo.PageVo;

import java.util.List;

/**
 * @description: req
 * @author: xiangbin
 * @create: 2023-05-13 14:33
 **/
@Data
@EqualsAndHashCode(callSuper = true)
public class TransactionReqVo extends PageVo {

    /**
     * erc20地址
     *
     * @required true
     * @mock 0x68533e9519f0997b06968dda7f7c9e58b5ba029a
     */
    private String erc20Address;

    /**
     * 交易类型1-swapErc20 2-swapEth 3-add 4-remove 5-burn
     *
     * @mock 3
     */
    private List<Integer> tradeType;

    /**
     * 开始时间
     *
     * @mock 1683993687
     */
    private Long startDate;

    /**
     * 结束时间
     *
     * @mock 1683993687
     */
    private Long endDate;

}
