package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;
import semios.api.model.entity.DaoStrategy;

/**
 * <p>
 * dao黑白名单策略表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface DaoStrategyMapper extends BaseMapper<DaoStrategy> {

    @Select("select * from dao_strategy where dao_id = #{daoId} and type = #{type} and strategy_type = #{strategyType} and is_valid = 1")
    DaoStrategy selectDaoStrategyByType(Integer daoId, Integer type, Integer strategyType);

    @Update("update dao_strategy set is_valid = 0 where dao_id = #{daoId} and type = #{type} and strategy_type = #{strategyType} and is_valid = 1")
    int updateDaoStrategyInvalid(Integer daoId, Integer type, Integer strategyType);

    @Select("select * from dao_strategy where transaction_hash = #{transactionHash} and type = #{type} and strategy_type = #{strategyType} and is_valid = 1")
    DaoStrategy selectDaoStrategyByTransactionHash(String transactionHash, Integer type, Integer strategyType);
}
