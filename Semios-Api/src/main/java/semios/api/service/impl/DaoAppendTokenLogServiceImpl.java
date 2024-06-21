package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.DaoAppendTokenLogMapper;
import semios.api.mapper.DaoMapper;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAppendTokenLog;
import semios.api.service.IDaoAppendTokenLogService;

/**
 * <p>
 * dao代币追加记录 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class DaoAppendTokenLogServiceImpl extends ServiceImpl<DaoAppendTokenLogMapper, DaoAppendTokenLog> implements IDaoAppendTokenLogService {


    @Autowired
    private DaoAppendTokenLogMapper daoAppendTokenLogMapper;

    @Autowired
    private DaoMapper daoMapper;


    @Override
    @Transactional(rollbackFor = RuntimeException.class)
    public int insertDaoAppendTokenLogAndUpdateDao(DaoAppendTokenLog daoAppendTokenLog, Dao dao) {
        int i = daoAppendTokenLogMapper.insert(daoAppendTokenLog);
        i += daoMapper.updateById(dao);
        return i;
    }

    @Override
    public DaoAppendTokenLog selectByTransactionHash(String transactionHash) {
        return daoAppendTokenLogMapper.selectByTransactionHash(transactionHash);
    }
}
