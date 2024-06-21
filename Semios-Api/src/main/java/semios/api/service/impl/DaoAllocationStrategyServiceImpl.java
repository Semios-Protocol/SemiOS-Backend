package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.DaoAllocationStrategyMapper;
import semios.api.model.entity.DaoAllocationStrategy;
import semios.api.service.IDaoAllocationStrategyService;

import java.util.List;

/**
 * <p>
 * dao分配策略表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class DaoAllocationStrategyServiceImpl extends ServiceImpl<DaoAllocationStrategyMapper, DaoAllocationStrategy> implements IDaoAllocationStrategyService {

    @Autowired
    private DaoAllocationStrategyMapper daoAllocationStrategyMapper;

    @Override
    public List<DaoAllocationStrategy> selectByOriginProjectIdAndType(String originProjectId, Integer type) {
        return daoAllocationStrategyMapper.selectByOriginProjectIdAndType(originProjectId, type);
    }

    @Override
    public Integer deleteByOriginProjectIdAndType(String originProjectId, Integer type) {
        return daoAllocationStrategyMapper.deleteByOriginProjectIdAndType(originProjectId, type);
    }

    @Override
    public List<DaoAllocationStrategy> selectByProjectIdAndType(String projectId, Integer tokenType) {
        return daoAllocationStrategyMapper.selectByProjectIdAndType(projectId, tokenType);
    }
}
