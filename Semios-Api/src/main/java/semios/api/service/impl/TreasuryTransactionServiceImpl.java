package semios.api.service.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.TreasuryTransactionMapper;
import semios.api.model.entity.TreasuryTransaction;
import semios.api.model.vo.res.TogetherDaoTreasuryTransactionVo;
import semios.api.service.ITreasuryTransactionService;

/**
 * <p>
 * 国库交易表 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-02-22
 */
@Service
public class TreasuryTransactionServiceImpl extends ServiceImpl<TreasuryTransactionMapper, TreasuryTransaction> implements ITreasuryTransactionService {

    @Autowired
    private TreasuryTransactionMapper treasuryTransactionMapper;

    @Override
    public Page<TogetherDaoTreasuryTransactionVo> getTreasuryTransactionList(IPage<TreasuryTransaction> page, String projectId) {
        // 通过钱包给sub dao打款用不用展示..
        return treasuryTransactionMapper.selectTogetherDaoTreasuryTransactionList(page, projectId);
    }
}
