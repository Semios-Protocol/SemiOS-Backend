package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import semios.api.model.entity.DaoAllocationStrategy;

import java.util.List;

/**
 * <p>
 * dao分配策略表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface DaoAllocationStrategyMapper extends BaseMapper<DaoAllocationStrategy> {


    List<DaoAllocationStrategy> selectByOriginProjectIdAndType(String originProjectId, Integer tokenType);


    Integer deleteByOriginProjectIdAndType(String originProjectId, Integer tokenType);

    List<DaoAllocationStrategy> selectByProjectIdAndType(String projectId, Integer tokenType);

}
