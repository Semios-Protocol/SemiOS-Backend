package semios.dex.service;

import semios.dex.model.dto.response.TransactionDto;

public interface SubscriberChainService {


    void handleTrade(TransactionDto transactionDto) throws Exception;
}
