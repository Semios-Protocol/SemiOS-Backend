package semios.subscription.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.model.entity.Transaction;

import java.util.List;

/**
 * <p>
 * transaction记录表 服务类
 * </p>
 *
 * @author xiangbin
 */
public interface ITransactionService extends IService<Transaction> {

    int addTransactionAndBlockHeight(List<Transaction> transactionList, BlockHeight blockHeight);


    List<Transaction> selectNoticeTransaction();

    List<Transaction> selectNoticeTransactionBySubId(Integer subId);

    Transaction selectNoticeTransactionByHash(String appName, String hash, Integer subId);

    List<Transaction> selectNoticeFailedTransactionBySubId(Integer subId);
}
