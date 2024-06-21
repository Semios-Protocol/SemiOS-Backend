package semios.api.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.mapper.WhiteListMapper;
import semios.api.model.entity.WhiteList;
import semios.api.service.IWhiteListService;

/**
 * <p>
 * 黑白名单地址表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class WhiteListServiceImpl extends ServiceImpl<WhiteListMapper, WhiteList> implements IWhiteListService {


    @Autowired
    private WhiteListMapper whiteListMapper;


    @Override
    public WhiteList selectByAddressAndRoot(String userAddress, String proofRootHash) {
        return whiteListMapper.selectByAddressAndRoot(userAddress, proofRootHash);
    }
}
