package semios.dex.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.dex.model.entity.Subscribe;

import java.util.List;

/**
 * <p>
 * 订阅记录 服务类
 * </p>
 *
 * @author xiangbin
 */
public interface ISubscribeService extends IService<Subscribe> {

    Subscribe selectByFilterId(String filterId);

    List<Subscribe> selectAll();

    List<Subscribe> selectByStatusStoped();

}
