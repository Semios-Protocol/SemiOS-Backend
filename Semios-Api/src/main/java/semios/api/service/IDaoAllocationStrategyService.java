package semios.api.service;

import semios.api.model.entity.DaoAllocationStrategy;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * dao分配策略表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IDaoAllocationStrategyService extends IService<DaoAllocationStrategy> {

    List<DaoAllocationStrategy> selectByOriginProjectIdAndType(String originProjectId, Integer type);

    Integer deleteByOriginProjectIdAndType(String originProjectId, Integer type);

    List<DaoAllocationStrategy> selectByProjectIdAndType(String projectId, Integer tokenType);
}
