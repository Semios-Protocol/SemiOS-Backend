package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.WorkTopupHarvest;
import semios.api.service.IDaoService;
import semios.api.service.IWorkTopupHarvestService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

@Slf4j
@Service
public class TopUpAccountUpdatedChainService implements SubscriberChainService {


    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;


    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    public static void main(String[] args) {

        String data = CommonUtil.removeHexPrefixIfExists("0x24283391c24fa8fbf9255ef16d3ff7c75b35ab9cf5603752e2f5c04e98ab2b3b000000000000000000000000a36a9e04edd5ce11e65bfa942be8386f3b49aea80000000000000000000000000000000000000000000000000000000000000003");
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String erc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String workNumber = CommonUtil.hexToTenString(dataList.get(2));

        log.info("projectId:" + projectId);
        log.info("erc721Address:" + erc721Address);
        log.info("workNumber:" + workNumber);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[TopUpAccountUpdatedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String erc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String workNumber = CommonUtil.hexToTenString(dataList.get(2));
        // 更新work_topup_harvest表中的on chain...(input token,output token amount)
        // 可能有问题...

        // project 可能是sub dao的project
        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[TopUpAccountUpdatedChainService] can not find dao:{}", projectId);
            throw new Exception("TopUpAccountUpdatedChainService dao is null");
        }

        String existDaoId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        WorkTopupHarvest workTopupHarvest = workTopupHarvestService.selectByProjectIdAndNft(existDaoId, erc721Address, workNumber);
        if (workTopupHarvest == null) {
            log.error("[TopUpAccountUpdatedChainService] workTopupHarvest is null,projectID:{},erc721Address:{},workNumber:{}", projectId, erc721Address, workNumber);
            throw new Exception("TopUpAccountUpdatedChainService workTopupHarvest is null");
        }

        WorkTopupHarvest updatedWorkTopupHarvest = commonService.updateOnChainWorkTopupHarvest(workTopupHarvest, dao);
        if (updatedWorkTopupHarvest == null) {
            log.error("[TopUpAccountUpdatedChainService] updateOnChainWorkTopupHarvest is null");
            throw new Exception("TopUpAccountUpdatedChainService updatedWorkTopupHarvest is null");
        }
        updatedWorkTopupHarvest.setId(workTopupHarvest.getId());
        log.info("[TopUpAccountUpdatedChainService] updatedWorkTopupHarvest:{}", JacksonUtil.obj2json(updatedWorkTopupHarvest));
        workTopupHarvestService.updateById(updatedWorkTopupHarvest);

    }
}
