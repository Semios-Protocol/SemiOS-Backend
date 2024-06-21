package semios.api.model.dto.chain;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.response.TransactionDto;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * @description: transactionDto
 * @author: xiangbin
 * @create: 2023-05-30 18:05
 **/
@Data
public class TransferErc20Dto {

    private String fromAddress;
    private String toAddress;
    private BigDecimal tokenNum;
    private TransactionDto transactionDto;

    public static TransferErc20Dto transfer(TransactionDto transactionDto) {
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String fromAddress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)).toLowerCase());
        String toAdress =
                CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(2)).toLowerCase());
        String amount = CommonUtil.hexToTenString(transactionDto.getData());
        BigDecimal tokenNum =
                new BigDecimal(amount).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18, RoundingMode.FLOOR);

        TransferErc20Dto transferErc20Dto = new TransferErc20Dto();
        transferErc20Dto.setFromAddress(fromAddress);
        transferErc20Dto.setToAddress(toAdress);
        transferErc20Dto.setTokenNum(tokenNum);
        transferErc20Dto.setTransactionDto(transactionDto);
        return transferErc20Dto;

    }
}
