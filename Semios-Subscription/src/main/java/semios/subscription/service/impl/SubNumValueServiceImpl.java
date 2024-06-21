package semios.subscription.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.subscription.mapper.SubNumValueMapper;
import semios.subscription.model.entity.SubNumValue;
import semios.subscription.service.ISubNumValueService;

/**
 * <p>
 * 数值类型订阅最新值 服务实现类
 * </p>
 *
 * @author xiangbin
 */
@Service
public class SubNumValueServiceImpl extends ServiceImpl<SubNumValueMapper, SubNumValue> implements ISubNumValueService {

    @Autowired
    private SubNumValueMapper subNumValueMapper;


    @Override
    public SubNumValue selectAddressAndTopic(String netWork, String address, String topic) {
        return subNumValueMapper.selectAddressAndTopic(netWork, address, topic);
    }

    @Override
    public SubNumValue selectByFilterId(Integer filterId) {
        return subNumValueMapper.selectByFilterId(filterId);
    }
}
