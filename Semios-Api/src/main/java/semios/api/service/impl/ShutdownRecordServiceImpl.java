package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.ShutdownRecordMapper;
import semios.api.model.entity.ShutdownRecord;
import semios.api.service.IShutdownRecordService;

/**
 * <p>
 * 停机记录表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class ShutdownRecordServiceImpl extends ServiceImpl<ShutdownRecordMapper, ShutdownRecord> implements IShutdownRecordService {

    @Autowired
    private ShutdownRecordMapper shutdownRecordMapper;

    @Override
    public ShutdownRecord selectByType(Integer type, String recordId) {
        return shutdownRecordMapper.selectByType(type, recordId);
    }
}
