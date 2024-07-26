package semios.api.service;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.NodePermissionNft;
import semios.api.model.vo.res.NodePermission.*;

import java.util.List;

/**
 * <p>
 * nft权限表 服务类
 * </p>
 *
 * @author zhyyao
 * @since 2024-07-16
 */
public interface INodePermissionNftService extends IService<NodePermissionNft> {

    NodePermissionInfo selectNodePermissionNft(Integer daoId, Integer permissionType);

    Page<NftDetailPermission> getNftPermissionList(IPage<NftDetailPermission> iPage, Integer workId);

    List<NodeDetailPermission> selectNodeDetailPermission(Integer daoId);

    Page<UserPermissionNft> selectUserPermissionNft(IPage<UserPermissionNft> iPage, String userAddress);

    Page<SelectPermissionNft> selectPermissionNftCount(IPage<SelectPermissionNft> iPage, String userAddress, Integer workId);

    NodePermissionNft selectNodePermissionNft(String projectId, Integer permissionType);

    void updateNodePermissionNft(NodePermissionNft nodePermissionNft);
}
