package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.WhiteList;

/**
 * <p>
 * 黑白名单地址表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IWhiteListService extends IService<WhiteList> {


    WhiteList selectByAddressAndRoot(String userAddress, String proofRootHash);
}
