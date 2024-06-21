package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TreasuryTransaction;
import semios.api.model.enums.TreasuryTransactionTypeEnum;
import semios.api.service.IDaoService;
import semios.api.service.ITreasuryTransactionService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.List;

/*
 * 账户给国库打款
 * address erc721Address, //打款后赠送的NFT的721地址
 * uint256 tokenId, //打款后赠送的NFT的token id
 * bytes32 daoId, //收款sub node的dao id
 * address granter, //打款的地址
 * uint256 grantAmount,  //打款的数量，乘了10**decimal
 * uint256 grantBlock, //打款的区块高度
 * address token //打款token的地址，即dao erc20地址
 * */
@Slf4j
@Service
public class NewSemiOsGrantTreasuryNftChainService implements SubscriberChainService {

    @Resource
    private IDaoService daoService;

    @Resource
    private ITreasuryTransactionService treasuryTransactionService;

    @Resource
    private CommonService commonService;

    public static void main(String[] args) {

    }

    // dao erc20 ---> 国库
    // from 合约抛出
    // to 国库地址
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewSemiOsGrantTreasuryNftChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String generateErc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(0))); //打款后赠送的NFT的721地址
        String generateTokenId = CommonUtil.hexToTenString(dataList.get(1)); //打款后赠送的NFT的token id
        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(2)); // //收款的sub node的project id (main dao)
        String fromAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));  // from address
        String grantAmount = CommonUtil.hexToTenString(dataList.get(4)); //打款的数量，乘了10**decimal
        String grantBlock = CommonUtil.hexToTenString(dataList.get(5)); //打款的区块高度
        String daoTokenAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(6))); //address erc721Address; 绑定的dao 721tokenAddress


        // main dao信息
        Dao costDao = daoService.daoDetailByProjectId(projectId);
        if (costDao == null) {
            log.error("[NewSemiOsGrantAssetPoolNftChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiOsGrantAssetPoolNftChainService cannot find dao");
        }

        BigDecimal amount = BigDecimal.ZERO;

        String decimals = commonService.erc20Decimals(daoTokenAddress);
        if (StringUtils.isBlank(decimals)) {
            log.error("[updateMinterWorkTopupHarvest] mountDao:{} getDecimals error", costDao.getId());
        } else {
            amount = new BigDecimal(grantAmount).divide(new BigDecimal("10").pow(Integer.parseInt(decimals)));
        }


        // 收款的dao信息
        Dao incomeDao = daoService.daoDetailByProjectId(projectId);
        if (incomeDao == null) {
            log.error("[NewSemiOsGrantAssetPoolNftChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiOsGrantAssetPoolNftChainService cannot find dao");
        }

        TreasuryTransaction treasuryTransaction = new TreasuryTransaction();

        treasuryTransaction.setProjectId(costDao.getProjectId());   // 打款dao的projectId
        treasuryTransaction.setTransactionHash(transactionDto.getTransactionHash());
        treasuryTransaction.setFromAddress(fromAddress);
        treasuryTransaction.setToAddress(CommonUtil.addHexPrefixIfNotExist(incomeDao.getTreasuryErc20()));    // 国库对应的erc20地址
        treasuryTransaction.setBlockNumber(grantBlock);
        treasuryTransaction.setGenerateErc721Address(generateErc721Address);
        treasuryTransaction.setGenerateTokenId(generateTokenId);
        treasuryTransaction.setAmount(amount);
        treasuryTransaction.setIsUseTreasury(0);

        treasuryTransaction.setTransactionType(TreasuryTransactionTypeEnum.TO_TREASURY.getStatus()); // 给sub dao打款
        treasuryTransaction.setSubDaoProjectId(projectId);
        treasuryTransactionService.save(treasuryTransaction);
    }
}
