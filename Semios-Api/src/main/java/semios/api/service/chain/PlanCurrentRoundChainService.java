package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.request.NoticeSubValueDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.entity.NftRewardAllocation;
import semios.api.model.enums.ContractMethodEnum;
import semios.api.model.enums.Plan.PlanStatusEnum;
import semios.api.model.vo.res.TopUpReward.TopupNftListVo;
import semios.api.service.*;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * plan round 变更事件
 *
 * @description:
 * @author: zhyyao
 * @create: 2024-05-05 13:43
 **/
@Slf4j
@Service
public class PlanCurrentRoundChainService implements SubscriberChainService {

    @Autowired
    private IIncentivePlanService iIncentivePlanService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private INftRewardAllocationService nftRewardAllocationService;

    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    public static void main(String[] args) {
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        throw new RuntimeException("PlanCurrentRoundChainService not exist handleTrade!");
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handleTradeValue(NoticeSubValueDto noticeSubValueDto) throws Exception {

        log.info("[PlanCurrentRoundChainService] noticeSubValueDto:{}", JacksonUtil.obj2json(noticeSubValueDto));

        String topics = noticeSubValueDto.getTopics();
        String planCode = topics.replace(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getMethodAddress(), "");

        String value = CommonUtil.hexToTenString(noticeSubValueDto.getValue());
        if (StringUtils.isBlank(value)) {
            log.error("[PlanCurrentRoundChainService] get current is error:{}", JacksonUtil.obj2json(noticeSubValueDto));
            throw new RuntimeException("PlanCurrentRoundChainService value is error");
        }

        IncentivePlan incentivePlan = iIncentivePlanService.getPlanByPlanCode(planCode);
        if (incentivePlan == null) {
            log.error("[PlanCurrentRoundChainService] cannot find plan:{}", JacksonUtil.obj2json(noticeSubValueDto));
            throw new RuntimeException("PlanCurrentRoundChainService cannot find plan");
        }

        Dao dao = daoService.daoDetailByProjectId(incentivePlan.getProjectId());
        if (dao == null) {
            log.error("[PlanCurrentRoundChainService] cannot find dao:{}", JacksonUtil.obj2json(noticeSubValueDto));
            throw new RuntimeException("PlanCurrentRoundChainService cannot find dao");
        }

        // 周期数没变化，不用查询
        Integer currentRound = Integer.valueOf(value); // commonService.getPlanCurrentRound(planCode);
        if (currentRound.equals(incentivePlan.getCurrentRound())) {
            log.info("[PlanCurrentRoundChainService] 周期没有变化，不做更改:{},currentRound:{}", planCode, currentRound);
            return;
        }


        IncentivePlan updatePlan = new IncentivePlan();
        updatePlan.setId(incentivePlan.getId());
        updatePlan.setCurrentRound(currentRound);


        if (currentRound <= incentivePlan.getPlanBlockWindow()) {
            // 当前周期数<=总周期
            updatePlan.setIncentiveStatus(PlanStatusEnum.STARTED.getBasicType());
        } else {
            // 当前周期数>总周期,已结束
            updatePlan.setIncentiveStatus(PlanStatusEnum.FINISHED.getBasicType());
            //  已经结束的，是否需要删除调订阅服务的订阅...
        }

        // 获取decimal
        // decimal要使用plan的decimal
        // Integer decimal = Integer.valueOf(commonService.erc20Decimals(dao.getErc20Token()));
        Integer decimal = incentivePlan.getRewardTokenDecimal();

        // 获取plan为每个nft分配了多少收益
        // 对seed nodes下的top-up 的持有者做激励的 ，
        List<TopupNftListVo> topupNftLists = workTopupHarvestService.getTopupRewardNftList(incentivePlan.getProjectId());

        log.info("[PlanCurrentRoundChainService] 需要从合约中获取的奖励nft列表为:{}", JacksonUtil.obj2json(topupNftLists));
        List<NftRewardAllocation> detailList = new ArrayList<>();

        for (TopupNftListVo topupNftListVo : topupNftLists) {
            BigDecimal amount = commonService.getPlanNftReward(planCode, topupNftListVo.getErc721Token(), topupNftListVo.getWorkNumber(), decimal);

            // daoId,workId,planCode确定唯一 一条记录
            NftRewardAllocation nftRewardAllocation = nftRewardAllocationService.getNftRewardAllocationByInfo(topupNftListVo.getDaoId(), topupNftListVo.getWorkId(), incentivePlan.getPlanCode());
            NftRewardAllocation newNftRewardAllocation = new NftRewardAllocation();
            if (nftRewardAllocation == null) {
                if (amount.compareTo(BigDecimal.ZERO) <= 0) {
                    // if compare to 0, then no need to save
                    continue;
                }
                newNftRewardAllocation.setDaoId(topupNftListVo.getDaoId());
                newNftRewardAllocation.setProjectId(topupNftListVo.getProjectId());
                newNftRewardAllocation.setWorkId(topupNftListVo.getWorkId());
                newNftRewardAllocation.setErc721Address(topupNftListVo.getErc721Token());
                newNftRewardAllocation.setWorkNumber(topupNftListVo.getWorkNumber());
                newNftRewardAllocation.setPlanCode(incentivePlan.getPlanCode());
            } else {
                newNftRewardAllocation.setId(nftRewardAllocation.getId());
            }
            newNftRewardAllocation.setPlanRewardAmount(amount); // 不用做加法，直接存入

            detailList.add(newNftRewardAllocation);

        }
        log.info("[PlanCurrentRoundChainService] update detailList:{}", JacksonUtil.obj2json(detailList));
        nftRewardAllocationService.saveOrUpdateBatch(detailList);

        // 计算剩余发放的总量
        updatePlan.setRemainingToken(incentivePlan.getIncentiveAmount().subtract(getPlanCumulativeReward(planCode, decimal)));
        log.info("[PlanCurrentRoundChainService] update plan:{}", JacksonUtil.obj2json(updatePlan));
        iIncentivePlanService.updateById(updatePlan);
    }

    // 获取plan发放的总共收益
    private BigDecimal getPlanCumulativeReward(String planCode, Integer decimal) {
        InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
        infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
        infuraCallRequestDto.setTo(ContractMethodEnum.PLAN_CUMULATED_REWARD.getContractAddress());
        infuraCallRequestDto.setData(ContractMethodEnum.PLAN_CUMULATED_REWARD.getMethodAddress() + CommonUtil.removeHexPrefixIfExists(planCode));

        Result<String> rewardResult = iSubscriptionService.infuraCall(infuraCallRequestDto);
        if (rewardResult.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
            log.info("[getPlanCumulativeReward]infura getPlanCumulatedReward return data:{}", rewardResult.getData());

            String amount = CommonUtil.hexToTenString(rewardResult.getData());
            return new BigDecimal(amount).divide(CommonUtil.getPowBigDecimal(decimal), 18, RoundingMode.HALF_UP);
        } else {
            log.error("[getPlanCumulativeReward]infura getPlanCumulatedReward return data:{} desc:{} planCode:{}",
                    rewardResult.getData(), rewardResult.getResultDesc(), planCode);
        }
        return BigDecimal.ZERO;
    }
}
