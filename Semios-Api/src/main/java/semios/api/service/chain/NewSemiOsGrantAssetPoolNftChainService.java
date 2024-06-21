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
 * 给sub node打款的记录信息
 * address erc721Address, //打款后赠送的NFT的721地址
 * uint256 tokenId, //打款后赠送的NFT的token id
 * bytes32 daoId, //收款 sub node的dao id
 * address granter, //打款的地址
 * uint256 grantAmount, //打款的数量，乘了10**decimal
 * bool isUseTreasury, //是否通过国库打款
 * uint256 grantBlock, //打款的区块高度
 * address token //打款 token的地址，即dao erc20地址
 * */
@Slf4j
@Service
public class NewSemiOsGrantAssetPoolNftChainService implements SubscriberChainService {

    @Resource
    private IDaoService daoService;

    @Resource
    private ITreasuryTransactionService treasuryTransactionService;

    @Resource
    private CommonService commonService;

    public static void main(String[] args) {

    }

    // 国库---> sub dao
    // from 合约抛出
    // to 收款dao的ass pool地址
    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewSemiOsGrantAssetPoolNftChainService.java] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String generateErc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(0))); //打款后赠送的NFT的721地址
        String generateTokenId = CommonUtil.hexToTenString(dataList.get(1)); //打款后赠送的NFT的token id
        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(2)); // //收款的sub node的project id (main dao)
        String granter = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(3)));  // from address
        String grantAmount = CommonUtil.hexToTenString(dataList.get(4)); //打款的数量，乘了10**decimal
        String isUseTreasury = CommonUtil.removeHexPrefixIfExists(dataList.get(5)); //是否通过国库打款
        String grantBlock = CommonUtil.hexToTenString(dataList.get(6)); //打款的区块高度
        String daoTokenAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(7))); //address erc721Address; 绑定的dao 721tokenAddress

        // main dao信息
        // 根据sub project找到该dao的main dao project
        Dao togetherDao = daoService.getTogetherDaoBySubDaoProjectId(projectId);
        if (togetherDao == null) {
            log.error("[NewSemiOsGrantAssetPoolNftChainService] togetherDao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiOsGrantAssetPoolNftChainService cannot find togetherDao");
        }
        Dao costDao = daoService.daoDetailByProjectId(togetherDao.getProjectId());
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

        treasuryTransaction.setProjectId(costDao.getProjectId());   // main dao的projectId
        treasuryTransaction.setTransactionHash(transactionDto.getTransactionHash());
        treasuryTransaction.setToAddress(CommonUtil.addHexPrefixIfNotExist(incomeDao.getFeePool()));    // sub dao的subdao asset pool地址
        treasuryTransaction.setBlockNumber(grantBlock);
        treasuryTransaction.setGenerateErc721Address(generateErc721Address);
        treasuryTransaction.setGenerateTokenId(generateTokenId);
        treasuryTransaction.setAmount(amount);
        treasuryTransaction.setIsUseTreasury(Integer.parseInt(isUseTreasury));

        // 如果用了国库，，就用国库的地址
        // 如果不用国库，，就用合约抛出的from..
        if (treasuryTransaction.getIsUseTreasury().equals(1)) {
            treasuryTransaction.setFromAddress(CommonUtil.addHexPrefixIfNotExist(costDao.getTreasuryErc20()));
        } else {
            treasuryTransaction.setFromAddress(granter);
        }

        treasuryTransaction.setTransactionType(TreasuryTransactionTypeEnum.TO_SUB_DAO.getStatus()); // 给sub dao打款
        treasuryTransaction.setSubDaoProjectId(projectId);


        treasuryTransactionService.save(treasuryTransaction);
    }
}
