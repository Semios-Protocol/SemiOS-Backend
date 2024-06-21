package semios.subscription.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.subscription.mapper.BlockHeightMapper;
import semios.subscription.mapper.TransactionMapper;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.model.entity.Transaction;
import semios.subscription.service.ITransactionService;

import java.util.List;

/**
 * <p>
 * transaction记录表 服务实现类
 * </p>
 *
 * @author xiangbin
 */
@Service
public class TransactionServiceImpl extends ServiceImpl<TransactionMapper, Transaction> implements ITransactionService {

    @Autowired
    private TransactionMapper transactionMapper;

    @Autowired
    private BlockHeightMapper blockHeightMapper;

    @Override
    @Transactional
    public int addTransactionAndBlockHeight(List<Transaction> transactionList, BlockHeight blockHeight) {

        int i = 0;
        for (Transaction transaction : transactionList) {
            i += transactionMapper.insert(transaction);
        }
        if (blockHeight.getId() == null) {
            i += blockHeightMapper.insert(blockHeight);
        } else {
            if (StringUtils.isBlank(blockHeight.getOriginBlock())) {
                i += blockHeightMapper.updateById(blockHeight);
            } else {
                i += blockHeightMapper.updateByBlock(blockHeight.getId(), blockHeight.getOriginBlock(), blockHeight.getToBlock());
            }
        }
        return i;
    }

    @Override
    public List<Transaction> selectNoticeTransaction() {
        return transactionMapper.selectNoticeTransaction();
    }

    @Override
    public List<Transaction> selectNoticeTransactionBySubId(Integer subId) {
        return transactionMapper.selectNoticeTransactionBySubId(subId);
    }

    @Override
    public Transaction selectNoticeTransactionByHash(String appName, String hash, Integer subId) {
        return transactionMapper.selectNoticeTransactionByHash(appName, hash, subId);
    }

    @Override
    public List<Transaction> selectNoticeFailedTransactionBySubId(Integer subId) {
        return transactionMapper.selectNoticeFailedTransactionBySubId(subId);
    }

}
