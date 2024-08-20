package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.DaoMapper;
import semios.api.mapper.DaoStrategyMapper;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoStrategy;
import semios.api.service.IDaoStrategyService;

import java.util.List;

/**
 * <p>
 * dao黑白名单策略表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class DaoStrategyServiceImpl extends ServiceImpl<DaoStrategyMapper, DaoStrategy> implements IDaoStrategyService {

    @Autowired
    private DaoStrategyMapper daoStrategyMapper;

    @Autowired
    private DaoMapper daoMapper;

    @Override
    public DaoStrategy selectDaoStrategyByType(Integer daoId, Integer type, Integer strategyType) {
        return daoStrategyMapper.selectDaoStrategyByType(daoId, type, strategyType);
    }

    @Transactional
    public int saveDaoStrategyAndInvalidOthers(DaoStrategy daoStrategy) {
        int num = daoStrategyMapper.updateDaoStrategyInvalid(daoStrategy.getDaoId(), daoStrategy.getType(),
                daoStrategy.getStrategyType());
        num += daoStrategyMapper.insert(daoStrategy);
        return num;
    }

    public DaoStrategy selectDaoStrategyByTransactionHash(String transactionHash, Integer type, Integer strategyType) {
        return daoStrategyMapper.selectDaoStrategyByTransactionHash(transactionHash, type, strategyType);
    }

    @Transactional
    @Override
    public int saveDaoStrategyOrUpdateDao(DaoStrategy daoStrategy, Dao dao) {
        int i = 0;
        if (daoStrategy != null) {
            if (daoStrategy.getId() != null) {
                i += daoStrategyMapper.updateById(daoStrategy);
            } else {
                i += daoStrategyMapper.insert(daoStrategy);
            }
        }
        if (dao != null) {
            i += daoMapper.updateById(dao);
        }
        return i;
    }

    @Transactional
    @Override
    public int saveDaoStrategyListOrUpdateDao(List<DaoStrategy> daoStrategyList, Dao dao) {
        int i = 0;
        if (dao != null && dao.getId() != null) {
            i += daoMapper.updateById(dao);
        } else {
            i += daoMapper.insert(dao);
        }

        if (daoStrategyList != null && daoStrategyList.size() > 0) {
            for (DaoStrategy daoStrategy : daoStrategyList) {
                if (dao != null && daoStrategy.getDaoId() == null) {
                    daoStrategy.setDaoId(dao.getId());
                }
                if (daoStrategy.getId() != null) {
                    i += daoStrategyMapper.updateById(daoStrategy);
                } else {
                    i += daoStrategyMapper.insert(daoStrategy);
                }
            }
        }

        return i;
    }


    @Override
    public List<DaoStrategy> selectDaoStrategyByDaoId(Integer daoId) {
        return daoStrategyMapper.selectDaoStrategyByDaoId(daoId);
    }
}
