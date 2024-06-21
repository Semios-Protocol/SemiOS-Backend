package semios.subscription.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.subscription.mapper.BlockHeightMapper;
import semios.subscription.mapper.SubscriberMapper;
import semios.subscription.model.entity.BlockHeight;
import semios.subscription.model.entity.Subscriber;
import semios.subscription.service.ISubscriberService;

import java.util.List;

/**
 * <p>
 * 订阅记录表 服务实现类
 * </p>
 *
 * @author xiangbin
 */
@Slf4j
@Service
public class SubscriberServiceImpl extends ServiceImpl<SubscriberMapper, Subscriber> implements ISubscriberService {

    @Autowired
    private SubscriberMapper subscriberMapper;

    @Autowired
    private BlockHeightMapper blockHeightMapper;

    @Override
    @Transactional
    public int saveSub(Subscriber subscriber) {
        int i = subscriberMapper.insert(subscriber);

        if (StringUtils.isNotBlank(subscriber.getFromBlock())) {
            BlockHeight blockHeight = new BlockHeight();
            blockHeight.setFromBlock(subscriber.getFromBlock());
            blockHeight.setToBlock(subscriber.getFromBlock());
            blockHeight.setSubId(subscriber.getId());
            i += blockHeightMapper.insert(blockHeight);
        }
        return i;
    }

    @Override
    public List<Subscriber> findAllOpenTenSub() {
        return subscriberMapper.findAllOpenTenSub();
    }

    @Override
    public List<Subscriber> findAllOpenSixtySub() {
        return subscriberMapper.findAllOpenSixtySub();
    }

    @Override
    public List<Subscriber> findSubByAppNameAnd(Subscriber subscriber) {
        return subscriberMapper.findSubByAppNameAnd(subscriber);
    }
}
