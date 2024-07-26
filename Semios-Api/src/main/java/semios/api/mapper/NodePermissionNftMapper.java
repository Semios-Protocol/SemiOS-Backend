package semios.api.mapper;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;
import semios.api.model.entity.NodePermissionNft;
import semios.api.model.vo.res.NodePermission.*;

import java.util.List;

/**
 * <p>
 * nft权限表 Mapper 接口
 * </p>
 *
 * @author zhyyao
 * @since 2024-07-16
 */
public interface NodePermissionNftMapper extends BaseMapper<NodePermissionNft> {

    @Select("select w.owner_address as ownerAddress,n.permissions_work_id as workId,d.dao_name as daoNameNft,n.permissions_erc721_token_id as workNumber,n.permissions_erc721_address as erc721Token " +
            " from (select * from node_permission_nft where dao_id=#{daoId} and permissions_type=#{permissionType} and is_valid=1 and is_del=0) n " +
            " inner join dao d on  n.permissions_dao_id = d.id " +
            " inner join work w on n.permissions_work_id = w.id ")
    NodePermissionInfo selectNodePermissionNft(Integer daoId, Integer permissionType);


    @Select("select n.permissions_type as permissionType, n.dao_id as daoId,d.dao_name as daoName,n.project_id as projectId " +
            " from (select * from node_permission_nft where permissions_work_id=#{workId} and is_valid=1 and is_del=0) n " +
            " inner join dao d on n.dao_id = d.id " +
            " order by n.create_time desc,n.id ")
    Page<NftDetailPermission> getNftPermissionList(IPage<NftDetailPermission> iPage, Integer workId);


    @Select("select n.permissions_type as permissionType,n.permissions_work_id as workId,d.dao_name as daoNameNft,n.project_id as projectId, " +
            "       n.permissions_erc721_token_id as workNumber,n.permissions_erc721_address as erc721Token, " +
            "       w.owner_address as ownerAddress " +
            " from (select * from node_permission_nft  where dao_id=#{daoId} and is_valid=1 and is_del=0) n " +
            " inner join dao d on n.permissions_dao_id = d.id " +
            " inner join work w on n.permissions_work_id = w.id order by n.create_time desc,n.id ")
    List<NodeDetailPermission> selectNodeDetailPermission(Integer daoId);


    @Select("select n.permissions_type as permissionType,d1.id as daoId,d1.dao_name as daoName, n.project_id as projectId," +
            " n.permissions_work_id as workId,d2.dao_name as daoNameNft,n.permissions_erc721_token_id as workNumber,n.permissions_erc721_address as erc721Token " +
            " from " +
            " (select * from node_permission_nft where is_del=0 and is_valid=1) n " +
            " inner join (select * from work where owner_address=#{userAddress} and work_status=1) w on w.id=n.permissions_work_id " +
            " inner join dao d1 on d1.id=n.dao_id " +
            " inner join dao d2 on d2.id=n.permissions_dao_id " +
            " order by n.create_time desc,n.id ")
    Page<UserPermissionNft> selectUserPermissionNft(IPage<UserPermissionNft> iPage, String userAddress);


    @Select("select any_value(w.image_url) as imgUrl,any_value(w.bg_color) as bgColor, any_value(w.height) as height,any_value(w.id) as workId," +
            " any_value(d.dao_name) as daoNameNft, " +
            " any_value(w.work_number) as workNumber,any_value(d.erc721_token) as erc721Token,count(n.permissions_work_id) as permissionCount " +
            " from (select * from work where owner_address=#{userAddress} and work_status=1 and id <> #{workId}) w " +
            " left join (select * from node_permission_nft where is_valid=1 and is_del=0) n on w.id=n.permissions_work_id " +
            " left join dao d on w.dao_id = d.id " +
            " group by w.id " +
            " order by permissionCount desc ")
    Page<SelectPermissionNft> selectPermissionNftCount(IPage<SelectPermissionNft> iPage, String userAddress, Integer workId);


    @Select("select * from node_permission_nft where project_id=#{projectId} and permissions_type=#{permissionType} and is_valid=1 and is_del=0 ")
    NodePermissionNft selectNodePermissionNftByProjectIdAndType(String projectId, Integer permissionType);

    @Update("UPDATE node_permission_nft set is_valid=0 where id=#{id} ")
    void deleteNodePermissionNft(Integer id);
}
