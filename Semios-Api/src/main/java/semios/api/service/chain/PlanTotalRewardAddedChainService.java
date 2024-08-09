package semios.api.service.chain;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.IncentivePlan;
import semios.api.model.entity.TreasuryTransaction;
import semios.api.model.enums.ContractMethodEnum;
import semios.api.model.enums.Plan.PlanAmountSourceEnum;
import semios.api.model.enums.TreasuryTransactionTypeEnum;
import semios.api.service.IDaoService;
import semios.api.service.IIncentivePlanService;
import semios.api.service.ITreasuryTransactionService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * plan追加token
 *
 * @description: plan追加token
 * @author: zhyyao
 * @create: 2024-05-01 13:43
 **/
@Slf4j
@Service
public class PlanTotalRewardAddedChainService implements SubscriberChainService {

    @Autowired
    private IIncentivePlanService iIncentivePlanService;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private ITreasuryTransactionService treasuryTransactionService;

    @Autowired
    private CommonService commonService;

    /*
    bytes32 planId, 
	uint256 amount, //追加的token数量
	bool useTreasury //是否使用国库
    **/

    @Override
    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[PlanTotalRewardAddedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String planId = dataList.get(0);
        String totalReward = CommonUtil.hexToTenString(dataList.get(1));    // 追加的token数量
        int isUseTreasury = Integer.parseInt(CommonUtil.hexToTenString(dataList.get(2)));   // 是否使用国库

        IncentivePlan incentivePlan = iIncentivePlanService.getPlanByPlanCode(planId);
        if (incentivePlan == null) {
            log.error("[PlanTotalRewardAddedChainService] cannot find plan:{}", planId);
            throw new RuntimeException("PlanTotalRewardAddedChainService cannot find plan");
        }

        Dao dao = daoService.daoDetailByProjectId(incentivePlan.getProjectId());
        if (dao == null) {
            log.error("[PlanTotalRewardAddedChainService] cannot find dao:{}", incentivePlan.getProjectId());
            throw new RuntimeException("PlanTotalRewardAddedChainService cannot find dao");
        }

        IncentivePlan updateIncentivePlan = new IncentivePlan();
        updateIncentivePlan.setId(incentivePlan.getId());

        // 获取decimal
        // 使用plan的decimal
        // Integer decimal = Integer.valueOf(commonService.erc20Decimals(dao.getErc20Token()));
        Integer decimal = incentivePlan.getRewardTokenDecimal();
        BigDecimal incentiveAmount = new BigDecimal(totalReward).divide(CommonUtil.getPowBigDecimal(decimal), 18, RoundingMode.HALF_UP);

        updateIncentivePlan.setIncentiveAmount(incentivePlan.getIncentiveAmount().add(incentiveAmount));  // 总奖励 = 之前的总量 +追加的
        updateIncentivePlan.setRemainingToken(incentivePlan.getRemainingToken().add(incentiveAmount)); // 剩余的奖励 = 之前剩余的 + 追加的


        // 如果使用了国库打钱，需要添加一条记录
        if (PlanAmountSourceEnum.TREASURY.getBasicType().equals(isUseTreasury)) {
            addRecordTreasury(dao, transactionDto, incentiveAmount);
        }
        log.info("[PlanTotalRewardAddedChainService] update plan:{}", JacksonUtil.obj2json(updateIncentivePlan));
        iIncentivePlanService.updateById(updateIncentivePlan);
    }

    private void addRecordTreasury(Dao dao, TransactionDto transactionDto, BigDecimal incentiveAmount) {
        TreasuryTransaction treasuryTransaction = new TreasuryTransaction();

        treasuryTransaction.setProjectId(dao.getProjectId());   // main dao的projectId
        treasuryTransaction.setTransactionHash(transactionDto.getTransactionHash());
        treasuryTransaction.setToAddress(ContractMethodEnum.GET_PLAN_CURRENT_ROUND.getContractAddress());    // 主合约地址
        treasuryTransaction.setBlockNumber(CommonUtil.hexToTenString(transactionDto.getBlockNumber()));
        treasuryTransaction.setGenerateErc721Address(null);
        treasuryTransaction.setGenerateTokenId(null);
        treasuryTransaction.setAmount(incentiveAmount);
        treasuryTransaction.setIsUseTreasury(1);    // 使用国库

        // 使用国库地址
        treasuryTransaction.setFromAddress(CommonUtil.addHexPrefixIfNotExist(dao.getTreasuryErc20()));
        treasuryTransaction.setTransactionType(TreasuryTransactionTypeEnum.TO_SUB_DAO.getStatus()); // 国库出钱
        treasuryTransaction.setSubDaoProjectId(null);

        log.info("[addRecordTreasury] insert treasury:{}", JacksonUtil.obj2json(treasuryTransaction));
        treasuryTransactionService.save(treasuryTransaction);
    }

}
