package semios.dex.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.dex.mapper.SubscribeMapper;
import semios.dex.model.entity.Subscribe;
import semios.dex.service.ISubscribeService;

import java.util.List;

/**
 * <p>
 * 订阅记录 服务实现类
 * </p>
 *
 * @author xiangbin
 */
@Service
public class SubscribeServiceImpl extends ServiceImpl<SubscribeMapper, Subscribe> implements ISubscribeService {

    @Autowired
    private SubscribeMapper subscribeMapper;


    @Override
    public Subscribe selectByFilterId(String filterId) {
        return subscribeMapper.selectByFilterId(filterId);
    }


    @Override
    public List<Subscribe> selectAll() {
        return subscribeMapper.selectAll();
    }

    @Override
    public List<Subscribe> selectByStatusStoped() {
        return subscribeMapper.selectByStatusStoped();
    }

}
