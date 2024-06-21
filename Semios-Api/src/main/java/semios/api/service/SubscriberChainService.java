package semios.api.service;

import semios.api.model.dto.request.NoticeSubValueDto;
import semios.api.model.dto.response.TransactionDto;

import java.util.List;

public interface SubscriberChainService {

    /**
     * 处理单个通知
     *
     * @param transactionDto
     * @throws Exception
     */
    void handleTrade(TransactionDto transactionDto) throws Exception;

    /**
     * 多个通知一起处理
     *
     * @param transactionDtoList
     */
    default void handleTradeList(List<TransactionDto> transactionDtoList) throws Exception {
        throw new RuntimeException("This method handleTradeList is not supported！");
    }

    /**
     * 处理监听到的数值变化事件
     *
     * @param noticeSubValueDto
     */
    default void handleTradeValue(NoticeSubValueDto noticeSubValueDto) throws Exception {
        throw new RuntimeException("This method handleTradealue is not supported！");
    }
}
