package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.ShutdownRecord;

/**
 * <p>
 * 停机记录表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IShutdownRecordService extends IService<ShutdownRecord> {


    ShutdownRecord selectByType(Integer type, String recordId);
}
