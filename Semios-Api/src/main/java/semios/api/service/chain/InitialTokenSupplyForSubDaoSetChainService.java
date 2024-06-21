package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAppendTokenLog;
import semios.api.service.IDaoAppendTokenLogService;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * 1.3 监听dao追加token 事件
 *
 * @description: 监听dao追加代币事件
 * @author: xiangbin
 * @create: 2022-08-25 11:40
 **/
@Slf4j
@Service
public class InitialTokenSupplyForSubDaoSetChainService implements SubscriberChainService {


    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private IDaoAppendTokenLogService daoAppendTokenLogService;

    public static void main(String[] args) {
        String data =
                "b2ec93de13a0b26ae10bcb168f6cc63cdc58d9413e79059e1ff564ab440d833e0000000000000000000000008405fee3e2a3b9a96c794a9237a33a6cbbc03221000000000000000000000000000000000000000000034f086f3b33b684000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String project_id = dataList.get(0);
        String erc20_token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1)));
        String amount = CommonUtil.hexToTenString(dataList.get(2));
        System.out.println("project_id" + " is " + project_id);
        System.out.println("erc20_token" + " is " + erc20_token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {
        log.info("[InitialTokenSupplyForSubDaoSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);
        String projectId = dataList.get(0);
        String initialTokenSupply = CommonUtil.hexToTenString(dataList.get(1));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            log.error("[InitialTokenSupplyForSubDaoSetChainService] dao is null transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            throw new RuntimeException("dao is null");
        }
        DaoAppendTokenLog daoAppendToken = daoAppendTokenLogService.selectByTransactionHash(transactionDto.getTransactionHash());
        if (daoAppendToken != null) {
            log.info("[InitialTokenSupplyForSubDaoSetChainService] append token erc20Address:{} transactionHash:{} had log", dao.getErc20Token(), transactionDto.getTransactionHash());
            return;
        }
        BigDecimal tokenNum =
                new BigDecimal(initialTokenSupply).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18, RoundingMode.FLOOR);

        if (tokenNum.compareTo(BigDecimal.ZERO) == 0) {
            log.info("[InitialTokenSupplyForSubDaoSetChainService] tokenNum is zero transactionDao:{}",
                    JacksonUtil.obj2json(transactionDto));
            return;
        }
        BigDecimal totalSupply = StringUtils.isBlank(dao.getErc20TotalSupply()) ? BigDecimal.ZERO : new BigDecimal(dao.getErc20TotalSupply());
        // 重新new update dao 去更新数据库，，如果用查询到的 dao 可能会有覆盖的问题.
        // dao.setErc20TotalSupply(tokenNum.add(totalSupply).toPlainString());
        // dao.setSubdaoAssetPoolBalance(commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals()).toPlainString());

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setErc20TotalSupply(tokenNum.add(totalSupply).toPlainString());
        updateDao.setSubdaoAssetPoolBalance(commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals()).toPlainString());

        DaoAppendTokenLog daoAppendTokenLog = new DaoAppendTokenLog();
        daoAppendTokenLog.setDaoId(dao.getId());
        daoAppendTokenLog.setProjectId(dao.getProjectId());
        daoAppendTokenLog.setOriginTotalSupply(totalSupply);
        daoAppendTokenLog.setNewTotalSupply(tokenNum.add(totalSupply));
//        daoAppendTokenLog.setUserAddress();
        daoAppendTokenLog.setIsThirdparty(0);
        daoAppendTokenLog.setTransactionHash(transactionDto.getTransactionHash());
        daoAppendTokenLog.setBlockTime(transactionDto.getBlockTime());


        int i = daoAppendTokenLogService.insertDaoAppendTokenLogAndUpdateDao(daoAppendTokenLog, updateDao);

        log.info("[InitialTokenSupplyForSubDaoSetChainService] update i:{}", i);
    }
}
