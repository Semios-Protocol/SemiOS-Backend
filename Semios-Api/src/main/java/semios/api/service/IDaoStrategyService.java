package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoStrategy;

import java.util.List;

/**
 * <p>
 * dao黑白名单策略表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IDaoStrategyService extends IService<DaoStrategy> {

    /**
     * 根据类型查询生效的策略
     *
     * @param daoId
     * @param type
     * @param strategyType
     * @return
     */
    DaoStrategy selectDaoStrategyByType(Integer daoId, Integer type, Integer strategyType);


    /**
     * 保存黑白名单策略+更新dao开启状态
     *
     * @param daoStrategy
     * @return
     */
    int saveDaoStrategyOrUpdateDao(DaoStrategy daoStrategy, Dao dao);

    /**
     * 保存黑白名单策略+更新dao开启状态
     *
     * @param daoStrategyList
     * @return
     */
    int saveDaoStrategyListOrUpdateDao(List<DaoStrategy> daoStrategyList, Dao dao);
}
