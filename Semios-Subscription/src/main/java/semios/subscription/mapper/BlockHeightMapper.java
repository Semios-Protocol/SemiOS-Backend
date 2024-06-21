package semios.subscription.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;
import semios.subscription.model.entity.BlockHeight;

/**
 * <p>
 * 订阅区块高度表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 */
public interface BlockHeightMapper extends BaseMapper<BlockHeight> {

    @Select("SELECT * from block_height where sub_id = #{subId};")
    BlockHeight selectBySubId(String subId);


    @Update("UPDATE block_height set to_block = #{toBlock} where id = #{id} and to_block = #{originBlock}")
    int updateByBlock(Integer id, String originBlock, String toBlock);


    @Update("UPDATE block_height set filter_id=NULL,to_block=NULL where id = #{id}")
    int updateFilterIdAndToBlockToNull(Integer id);
}
