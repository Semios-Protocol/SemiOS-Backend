package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import semios.api.model.entity.ShutdownRecord;

/**
 * <p>
 * 停机记录表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ShutdownRecordMapper extends BaseMapper<ShutdownRecord> {


    ShutdownRecord selectByType(Integer type, String recordId);

}
