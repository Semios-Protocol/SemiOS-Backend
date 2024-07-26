package semios.api.service.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.NodePermissionNftMapper;
import semios.api.model.entity.NodePermissionNft;
import semios.api.model.vo.res.NodePermission.*;
import semios.api.service.INodePermissionNftService;

import java.util.List;

/**
 * <p>
 * nft权限表 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-07-16
 */
@Service
public class NodePermissionNftServiceImpl extends ServiceImpl<NodePermissionNftMapper, NodePermissionNft> implements INodePermissionNftService {


    @Autowired
    private NodePermissionNftMapper nodePermissionNftMapper;


    @Override
    public NodePermissionInfo selectNodePermissionNft(Integer daoId, Integer permissionType) {
        return nodePermissionNftMapper.selectNodePermissionNft(daoId, permissionType);
    }

    @Override
    public Page<NftDetailPermission> getNftPermissionList(IPage<NftDetailPermission> iPage, Integer workId) {
        return nodePermissionNftMapper.getNftPermissionList(iPage, workId);
    }

    @Override
    public List<NodeDetailPermission> selectNodeDetailPermission(Integer daoId) {
        return nodePermissionNftMapper.selectNodeDetailPermission(daoId);
    }

    @Override
    public Page<UserPermissionNft> selectUserPermissionNft(IPage<UserPermissionNft> iPage, String userAddress) {
        return nodePermissionNftMapper.selectUserPermissionNft(iPage, userAddress);
    }

    @Override
    public Page<SelectPermissionNft> selectPermissionNftCount(IPage<SelectPermissionNft> iPage, String userAddress, Integer workId) {
        return nodePermissionNftMapper.selectPermissionNftCount(iPage, userAddress, workId);
    }

    @Override
    public NodePermissionNft selectNodePermissionNft(String projectId, Integer permissionType) {
        return nodePermissionNftMapper.selectNodePermissionNftByProjectIdAndType(projectId, permissionType);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateNodePermissionNft(NodePermissionNft nodePermissionNft) {
        // 原有的绑定关系解除
        nodePermissionNftMapper.deleteNodePermissionNft(nodePermissionNft.getId());

        // 新增绑定关系
        nodePermissionNft.setId(null);
        nodePermissionNft.setCreateTime(null);
        nodePermissionNft.setUpdateTime(null);
        nodePermissionNftMapper.insert(nodePermissionNft);
    }

}
