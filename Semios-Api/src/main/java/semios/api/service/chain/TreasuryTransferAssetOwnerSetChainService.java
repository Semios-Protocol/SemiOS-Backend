package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.NodePermissionNft;
import semios.api.model.entity.Work;
import semios.api.model.enums.NodePermissionTypeEnum;
import semios.api.model.enums.WorkStatusEnum;
import semios.api.service.IDaoService;
import semios.api.service.INodePermissionNftService;
import semios.api.service.IWorkService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;
import java.util.Objects;

/**
 * 转移国库的分配权限(仅main dao)
 *
 * @description: 转移国库的分配权限(仅main dao)
 * @author: zhyyao
 * @create: 2024-07-17 13:43
 **/
@Slf4j
@Service
public class TreasuryTransferAssetOwnerSetChainService implements SubscriberChainService {


    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkService workService;

    @Autowired
    private INodePermissionNftService nodePermissionNftService;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[TreasuryTransferAssetOwnerSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));
        String erc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String tokenId = CommonUtil.hexToTenString(dataList.get(2));

        // 查看数据库中目前绑定的信息
        NodePermissionNft nodePermissionNft = nodePermissionNftService.selectNodePermissionNft(projectId, NodePermissionTypeEnum.Treasury_Allocation.getType());
        if (nodePermissionNft == null) {
            log.error("[TreasuryTransferAssetOwnerSetChainService] nodePermissionNft is null, projectId:{},permissionType:{}", projectId, NodePermissionTypeEnum.Treasury_Allocation.getType());
            throw new Exception("nodePermissionNft is null");
        }
        if (nodePermissionNft.getTransactionHash().equalsIgnoreCase(transactionDto.getTransactionHash())){
            // 如果绑定的nft和本次绑定的nft一致,则无需修改
            log.info("[TreasuryTransferAssetOwnerSetChainService] nodePermissionNft is equal, projectId:{}", projectId);
            return;
        }

        // 查询本次绑定nft所属的node
        Dao dao = daoService.selectDaoByErc721Token(erc721Address);
        if (dao == null) {
            log.error("[TreasuryTransferAssetOwnerSetChainService] dao is null, erc721Address:{}", erc721Address);
            throw new Exception("dao is null");
        }
        // 查询数据库中的nft
        Work work = workService.selectWorkByNumber(dao.getId(), tokenId);
        if (work == null || !Objects.equals(work.getWorkStatus(), WorkStatusEnum.CASTED.getStatus())) {
            log.error("[TreasuryTransferAssetOwnerSetChainService] work is null, dao id:{}, tokenId:{}", dao.getId(), tokenId);
            throw new Exception("work is null");
        }


        // 修改需要调整的值,id不用动
        nodePermissionNft.setPermissionsDaoId(dao.getId());
        nodePermissionNft.setPermissionsWorkId(work.getId());
        nodePermissionNft.setPermissionsErc721Address(erc721Address);
        nodePermissionNft.setPermissionsErc721TokenId(tokenId);
        nodePermissionNft.setTransactionHash(transactionDto.getTransactionHash());
        nodePermissionNftService.updateNodePermissionNft(nodePermissionNft);
    }


}
