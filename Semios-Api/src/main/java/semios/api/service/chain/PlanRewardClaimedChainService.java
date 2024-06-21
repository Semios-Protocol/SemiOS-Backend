package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.*;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

@Slf4j
@Service
public class PlanRewardClaimedChainService implements SubscriberChainService {

    @Autowired
    private IIncentivePlanService iIncentivePlanService;

    @Autowired
    private ICollectRecordService collectRecordService;

    @Autowired
    private INftRewardAllocationService nftRewardAllocationService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Autowired
    private CommonService commonService;

    /*
    bytes32 planId,
	NftIdentifier nft,
	address owner, //收款人
	uint256 reward, //claim出的token数量（乘以decimal之后的）
	address token	//奖励token的地址
    **/
    @Override
    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[PlanRewardClaimedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String planId = dataList.get(0);
        String erc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String workNumber = CommonUtil.hexToTenString(dataList.get(2));
        String receivedAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));
        String totalClaimed = CommonUtil.hexToTenString(dataList.get(4));    // claimed的token数量

        IncentivePlan incentivePlan = iIncentivePlanService.getPlanByPlanCode(planId);
        if (incentivePlan == null) {
            log.error("[PlanRewardClaimedChainService] cannot find plan:{}", planId);
            throw new RuntimeException("PlanRewardClaimedChainService cannot find plan");
        }

        Dao dao = daoService.daoDetailByProjectId(incentivePlan.getProjectId());    // mint的erc720 address
        // seed node project id和work number怎么确定一个work
        if (dao == null) {
            log.error("[PlanRewardClaimedChainService] cannot find dao,erc721:{}", erc721Address);
            throw new RuntimeException("PlanRewardClaimedChainService cannot find dao");
        }

        WorkTopupHarvest workTopupHarvest = workTopupHarvestService.selectOneByNft(erc721Address, workNumber);
        if (workTopupHarvest == null) {
            log.error("[PlanRewardClaimedChainService] cannot find work:{}", JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("PlanRewardClaimedChainService cannot find work");
        }

        // TODO 使用plan的decimal
        // Integer decimal = Integer.valueOf(commonService.erc20Decimals(dao.getErc20Token()));
        Integer decimal = incentivePlan.getRewardTokenDecimal();
        log.info("[PlanRewardClaimedChainService] decimal:{}", decimal);
        BigDecimal collectAmount = new BigDecimal(totalClaimed).divide(CommonUtil.getPowBigDecimal(decimal), 18, RoundingMode.HALF_UP);
        log.info("[PlanRewardClaimedChainService] collectAmount:{}", collectAmount);

        if (collectAmount.compareTo(BigDecimal.ZERO) <= 0) {
            // 如果 collect amount为0，直接返回，不用记录
            log.info("[PlanRewardClaimedChainService] collectAmount is 0,return:{}", JacksonUtil.obj2json(transactionDto));
            return;
        }

        NftRewardAllocation nftRewardAllocation = nftRewardAllocationService.getNftRewardAllocationByInfo(incentivePlan.getDaoId(), workTopupHarvest.getMountWorkId(), incentivePlan.getPlanCode());
        if (nftRewardAllocation == null || (nftRewardAllocation.getPlanRewardAmount().compareTo(collectAmount)) < 0) {
            log.error("[PlanRewardClaimedChainService] cannot find nft reward or insufficient balance:{}", JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("PlanRewardClaimedChainService cannot find nft reward or insufficient balance");
        }

        log.info("[PlanRewardClaimedChainService] start save collectRecord:{}", JacksonUtil.obj2json(nftRewardAllocation));
        CollectRecord collectRecord = new CollectRecord();
        collectRecord.setDaoId(incentivePlan.getDaoId());
        collectRecord.setProjectId(incentivePlan.getProjectId());
        collectRecord.setPlanCode(incentivePlan.getPlanCode());
        collectRecord.setWorkId(workTopupHarvest.getMountWorkId());
        collectRecord.setErc721Address(erc721Address);
        collectRecord.setWorkNumber(Integer.parseInt(workNumber));
        collectRecord.setTransactionHash(transactionDto.getTransactionHash());
        collectRecord.setReceivedAddress(receivedAddress);
        collectRecord.setCollectAmount(collectAmount);
        collectRecord.setCreateBy(CommonUtil.addHexPrefixIfNotExist(transactionDto.getAddress()));

        log.info("[PlanRewardClaimedChainService] insert collect record:{}", JacksonUtil.obj2json(collectRecord));
        // 保存记录
        collectRecordService.save(collectRecord);


        // 修改可以领取的奖励
        NftRewardAllocation updateNftRewardAllocation = new NftRewardAllocation();
        updateNftRewardAllocation.setId(nftRewardAllocation.getId());
        BigDecimal amount = commonService.getPlanNftReward(planId, erc721Address, Integer.parseInt(workNumber), decimal);
        log.info("[PlanRewardClaimedChainService] getPlanNftReward:{}", amount);
        updateNftRewardAllocation.setPlanRewardAmount(amount);  // 从合约中现查询
        // updateNftRewardAllocation.setPlanRewardAmount(nftRewardAllocation.getPlanRewardAmount().subtract(collectAmount));
        log.info("[PlanRewardClaimedChainService] update nftReward:{}", JacksonUtil.obj2json(updateNftRewardAllocation));
        nftRewardAllocationService.updateById(updateNftRewardAllocation);
    }
}
