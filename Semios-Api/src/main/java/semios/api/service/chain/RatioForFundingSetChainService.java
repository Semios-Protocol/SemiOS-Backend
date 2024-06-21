package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.ProtoDaoCommonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * 1.3 修改链上参数
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class RatioForFundingSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    public static void main(String[] args) {
        String data = "58d2d3c7a28441df4656fab3eb0e51d94a45b38100a7e74dba50afae6124d6d72ae972c0263f681f4f35a5fbffcb8860ddb2ceb32ab4e3a4d8f6912888dca8c8000000000000000000000000ffd23ddffa1d6181c8a711a8b4939eedf9cc00bd0000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String amount = CommonUtil.hexToTenString(dataList.get(3));
        System.out.println("project_id" + " is " + projectId);
        System.out.println("canvas_id" + " is " + canvasId);
        System.out.println("erc20_token" + " is " + erc20Token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[RatioForFundingSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("RatioForFundingSetChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());

        //DAO Price Reserve Ratio
        String canvasCreatorMintFeeRatio = CommonUtil.hexToTenString(dataList.get(1));// Builder Mint Fee
        String assetPoolMintFeeRatio = CommonUtil.hexToTenString(dataList.get(2));// SubDAO Mint Fee
        String redeemPoolMintFeeRatio = CommonUtil.hexToTenString(dataList.get(3));// MainDAO Mint Fee
        String treasuryMintFeeRatio = CommonUtil.hexToTenString(dataList.get(4));// treasuryMintFeeRatio
        // 以上三个参数加 PAO Mint Fee 等于 10000


        // Fixed Price Reserve Ratio
        String canvasCreatorMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(5));// Builder Mint Fee
        String assetPoolMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(6));// SubDAO Mint Fee
        String redeemPoolMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(7));// MainDAO Mint Fee
        String treasuryMintFeeRatioFiatPrice = CommonUtil.hexToTenString(dataList.get(8));
        // 以上三个参数加 PDAO Mint Fee 等于 10000

        //Royalty Token
        String minterERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(9));// Minter Reward
        String canvasCreatorERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(10));// Builder Reward
        String daoCreatorERC20RewardRatio = CommonUtil.hexToTenString(dataList.get(11));// SubDAO Starter Reward
        // 以上三个参数加 PDAO Reward 等于 10000

        // ETH
        String minterETHRewardRatio = CommonUtil.hexToTenString(dataList.get(12));// Minter Reward
        String canvasCreatorETHRewardRatio = CommonUtil.hexToTenString(dataList.get(13));// Builder Reward
        String daoCreatorETHRewardRatio = CommonUtil.hexToTenString(dataList.get(14));// SubDAO Starter Reward
        // 以上三个参数加 PDAO Reward 等于 10000

        //DAO Price Reserve Ratio
        DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            daoRoyaltyToken = JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class);
        }
        daoRoyaltyToken.setDaoReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorERC20RewardRatio));
        daoRoyaltyToken.setCanvasReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorERC20RewardRatio));
        daoRoyaltyToken.setMinterReward(ProtoDaoCommonUtil.strToBigDecimal(minterERC20RewardRatio));
        daoRoyaltyToken.setD4aReward(new BigDecimal("100").subtract(daoRoyaltyToken.getDaoReward()).subtract(daoRoyaltyToken.getCanvasReward()).subtract(daoRoyaltyToken.getMinterReward()));

        //ETH
        DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
        if (StringUtils.isNotBlank(dao.getEthRoyaltyToken())) {
            daoEthRoyaltyToken = JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class);
        }
        daoEthRoyaltyToken.setMinterETHReward(ProtoDaoCommonUtil.strToBigDecimal(minterETHRewardRatio));
        daoEthRoyaltyToken.setCanvasCreatorETHReward(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorETHRewardRatio));
        daoEthRoyaltyToken.setDaoCreatorETHReward(ProtoDaoCommonUtil.strToBigDecimal(daoCreatorETHRewardRatio));
        daoEthRoyaltyToken.setD4aReward(new BigDecimal("100").subtract(daoEthRoyaltyToken.getMinterETHReward()).subtract(daoEthRoyaltyToken.getCanvasCreatorETHReward()).subtract(daoEthRoyaltyToken.getDaoCreatorETHReward()));

        // 非一口价
        DaoReserveRatio daoReserveRatio = new DaoReserveRatio(true);
        if (StringUtils.isNotBlank(dao.getFixedReserveRatio())) {
            daoReserveRatio = JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class);
        }
        daoReserveRatio.setCanvasMintFee(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorMintFeeRatio));
        daoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(assetPoolMintFeeRatio));
        daoReserveRatio.setRedeemPoolMintFee(ProtoDaoCommonUtil.strToBigDecimal(redeemPoolMintFeeRatio));
        daoReserveRatio.setD4aMintFee(new BigDecimal("100").subtract(daoReserveRatio.getDaoMintFee()).subtract(daoReserveRatio.getCanvasMintFee()).subtract(daoReserveRatio.getRedeemPoolMintFee()));
        // 一口价
        DaoReserveRatio fixedDaoReserveRatio = new DaoReserveRatio(false);
        if (StringUtils.isNotBlank(dao.getRoyaltyToken())) {
            fixedDaoReserveRatio = JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class);
        }
        fixedDaoReserveRatio.setCanvasMintFee(ProtoDaoCommonUtil.strToBigDecimal(canvasCreatorMintFeeRatioFiatPrice));
        fixedDaoReserveRatio.setDaoMintFee(ProtoDaoCommonUtil.strToBigDecimal(assetPoolMintFeeRatioFiatPrice));
        fixedDaoReserveRatio.setRedeemPoolMintFee(ProtoDaoCommonUtil.strToBigDecimal(redeemPoolMintFeeRatioFiatPrice));
        fixedDaoReserveRatio.setD4aMintFee(new BigDecimal("100").subtract(fixedDaoReserveRatio.getDaoMintFee()).subtract(fixedDaoReserveRatio.getCanvasMintFee()).subtract(fixedDaoReserveRatio.getRedeemPoolMintFee()));


        updateDao.setRoyaltyToken(JacksonUtil.obj2json(daoRoyaltyToken));
        updateDao.setEthRoyaltyToken(JacksonUtil.obj2json(daoEthRoyaltyToken));
        updateDao.setUnfixedReserveRatio(JacksonUtil.obj2json(daoReserveRatio));
        updateDao.setFixedReserveRatio(JacksonUtil.obj2json(fixedDaoReserveRatio));

        daoService.updateById(updateDao);

        log.info("[RatioForFundingSetChainService] updateDao:{}", JacksonUtil.obj2json(updateDao));

    }
}
