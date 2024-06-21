package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * 给 to 地址发送了 amount 数量的 ETH，to 地址为 D4AFeePool 或者每个 DAO 的 FeePool
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-03-10 13:43
 **/
@Slf4j
@Service
public class EthTransferedChainService implements SubscriberChainService {

    // 例子： 暂无

    @Autowired
    private IDaoService daoService;

    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    @Autowired
    private CommonService commonService;

    /**
     * 监听dao的splitter合约地址
     *
     * @param transactionDto 参数
     * @throws Exception 异常
     */
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[EthTransferedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        Dao dao = daoService.selectDaoBySplitterAddress(transactionDto.getContractAddress());
        if (dao == null) {
            throw new RuntimeException("EthTransferedChainService cannot find dao");
        }

        String amountStr = CommonUtil.hexToTenString(transactionDto.getData());
        BigDecimal amount =
                new BigDecimal(amountStr).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);
        List<String> topics = JacksonUtil.json2StringList(transactionDto.getTopics());
        String address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(topics.get(1)));

        if (!(address.equalsIgnoreCase(dao.getFeePool()))) {
            log.info("[EthTransferedChainService] daoId:{} address:{} to:{}", dao.getId(), address, amount);
            return;
        }
        // dao assetPool
//        Result<String> result = iSubscriptionService.ethGetBalance(ProtoDaoConstant.netWork, CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
//        if (result.getResultCode() == ResultDesc.SUCCESS.getResultCode()) {
//            log.info("[EthTransferedChainService]infura ethGetBalance return data:{}", result.getData());
//            String balance = result.getData();
//            String price = CommonUtil.hexToTenString(balance);
//            if (StringUtils.isNotBlank(price)) {
//                BigDecimal assetPool =
//                        new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()),18, RoundingMode.FLOOR);
//                log.info("[EthTransferedChainService]daoId:{} assetPool:{}", dao.getId(), assetPool);
//                dao.setDaoAssetPool(assetPool);
//            }
//        } else {
//            log.error("[EthTransferedChainService]infura ethGetBalance return data:{} daoId:{}", result.getData(),
//                    dao.getId());
//        }
        dao.setDaoAssetPool(commonService.getInputToken(dao));

        synchronized (EthTransferedChainService.class) {
            if (dao.getRoyaltyFeeIncome() == null) {
                log.info("[EthTransferedChainService] daoId:{} royalty_fee_income from null to:{}", dao.getId(),
                        amount);
                dao.setRoyaltyFeeIncome(amount);
                boolean i = daoService.updateById(dao);
                log.info("[EthTransferedChainService] daoId:{} return i:{}", dao.getId(), i);
            } else {
                log.info("[EthTransferedChainService] daoId:{} royalty_fee_income from:{} to:{}", dao.getId(),
                        dao.getRoyaltyFeeIncome(), dao.getRoyaltyFeeIncome().add(amount));
                int i = daoService.updateDaoRoyaltyFeeIncome(dao.getId(), dao.getRoyaltyFeeIncome(),
                        dao.getRoyaltyFeeIncome().add(amount));
                log.info("[EthTransferedChainService] daoId:{} return i:{}", dao.getId(), i);
            }
        }
    }

}
