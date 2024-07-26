package semios.api.model.dto.response;

import lombok.Data;
import org.web3j.protocol.core.methods.response.Log;
import semios.api.utils.BeanUtil;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.io.Serializable;

/**
 * <p>
 * transaction记录表
 * </p>
 *
 * @author xiangbin
 * @create: 2022-04-14 13:41
 */
@Data
public class TransactionDto implements Serializable {

    private static final long serialVersionUID = 1L;

    private String address;

    private String blockHash;

    private String blockNumber;

    private Integer blockIntNum;// 未使用

    private String data;

    private String logIndex;

    private String removed;

    private String topics;

    private String transactionHash;

    private String transactionIndex;

    private Integer subId;


    private String blockTime;

    // contract在自己创建的订阅中有使用到 其他订阅不使用
    // EthTransferedChainService.java   作为splitterAddress使用
    // TransferChainService.java   作为erc721Token使用
    // TransferErc20ChainService.java  作为erc720Token使用
    private String contractAddress;


    public TransactionDto transferTransactionDto(Log log) {
        TransactionDto transactionDto = new TransactionDto();
        BeanUtil.copyProperties(log, transactionDto);
        transactionDto.setBlockNumber(CommonUtil.addHexPrefixIfNotExist(log.getBlockNumberRaw()));
        transactionDto.setBlockIntNum(Integer.parseInt(CommonUtil.removeHexPrefixIfExists(log.getBlockNumberRaw()), 16));
        transactionDto.setRemoved(log.isRemoved() ? "1" : "0");
        transactionDto.setTopics(JacksonUtil.obj2json(log.getTopics()));
        // blockTime 在外层赋值
        return transactionDto;
    }
}
