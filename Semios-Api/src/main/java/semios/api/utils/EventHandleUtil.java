package semios.api.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.protocol.core.methods.response.Log;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.enums.TradeTypeEnum;
import semios.api.service.SubscriberChainService;

import java.util.List;
import java.util.stream.Collectors;

public class EventHandleUtil {
    private static final Logger logger = LoggerFactory.getLogger(EventHandleUtil.class);

    public static List<Log> getLog(List<Log> logs, String topic) {
        return logs.stream()
                .filter(log -> log.getTopics() != null && !log.getTopics().isEmpty() && log.getTopics().get(0).equals(CommonUtil.addHexPrefixIfNotExist(topic)))
                .collect(Collectors.toList());
    }

    public static Boolean handleEvent(List<Log> logs, TradeTypeEnum tradeTypeEnum, TransactionDto transactionDto) throws Exception {
        List<Log> receiptLogs = getLog(logs, tradeTypeEnum.getTopic());
        if (receiptLogs.isEmpty()) {
            logger.error("not found receipt topic=" + tradeTypeEnum.getTopic());
            return false;
        }
        logger.info(String.format("handleEvent handle=%s,\t receiptLogs=%s", tradeTypeEnum.getTradeServiceName(), JacksonUtil.obj2json(receiptLogs)));

        SubscriberChainService subscriberChainService =
                SpringBeanUtil.getBeanByName(tradeTypeEnum.getTradeServiceName(), SubscriberChainService.class);
        if (subscriberChainService == null) {
            logger.error("not found subscriberChainService=" + tradeTypeEnum.getTradeServiceName());
            return false;
        }
        // handle event
        for (Log log : receiptLogs) {
            TransactionDto transactionDto1 = new TransactionDto().transferTransactionDto(log);
            transactionDto1.setBlockTime(transactionDto.getBlockTime());
            subscriberChainService.handleTrade(transactionDto1);
        }
        return true;
    }
}
