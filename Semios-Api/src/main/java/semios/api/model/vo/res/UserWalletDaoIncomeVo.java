package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.web3j.abi.TypeDecoder;
import org.web3j.abi.datatypes.Type;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.entity.*;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.*;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.EthTransactionUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * 用户钱包展示已领取token
 *
 * @description: wallet income
 * @author: xiangbin
 * @create: 2022-08-08 11:20
 **/
@Data
@Slf4j
public class UserWalletDaoIncomeVo {

    /**
     * canvasId 1.9之前的dao
     */
    private List<String> canvasIdList = new ArrayList<>();

    /**
     * daoId 1.9之前的dao
     */
    private List<String> projectIdList = new ArrayList<>();

    /**
     * canvasId 1.9之后的dao
     */
    private List<String> canvasId2List = new ArrayList<>();

    /**
     * daoId 1.9之后的dao
     */
    private List<String> projectId2List = new ArrayList<>();

    /**
     * 未领取列表
     */
    private List<DaoIncomeVo> daoIncomeVos = new ArrayList<>();

    public static DaoIncomeVo transfer(List<Object> objectList, String userAddress) {

        DaoIncomeVo daoIncomeVo = new DaoIncomeVo();
        List<MintReward> mintRewardList = new ArrayList<>();
        List<Erc20Collected> erc20CollectedList = new ArrayList<>();
        List<Erc20Collectable> erc20CollectableList = new ArrayList<>();

        for (Object o : objectList) {
            if (o instanceof Dao) {
                Dao dao = (Dao) o;
                //钱包这里topup模式的不展示
                if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
                    continue;
                }
                daoIncomeVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));
                daoIncomeVo.setDaoName(dao.getDaoName());
                daoIncomeVo.setDaoVersion(dao.getDaoVersion());
                daoIncomeVo.setDaoNumber(dao.getDaoNumber());
                daoIncomeVo.setDaoSymbol(dao.getDaoSymbol());
                daoIncomeVo.setPayCurrencyType(dao.getPayCurrencyType());
                daoIncomeVo.setInputTokenAddress(dao.getInputTokenAddress());
                daoIncomeVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
                daoIncomeVo.setInputTokenDecimals(dao.getInputTokenDecimals());

                daoIncomeVo.setIsPaused(dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
                if (dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                    daoIncomeVo.setPauseedMsg(dao.getDaoName() + " (D4A@" + dao.getDaoNumber()
                            + ") This function is suspended for security reasons.");
                }
                if (dao.getSwapEth() != null) {
                    daoIncomeVo.setErc20Revenue(dao.getSwapEth());
                }

                // 查询当天兑换的记录
                ITokenReceivedRecordService tokenReceivedRecordService =
                        SpringBeanUtil.getBean(ITokenReceivedRecordService.class);
                if (tokenReceivedRecordService != null) {
                    List<TokenReceivedRecord> tokenReceivedRecords =
                            tokenReceivedRecordService.recordListByTokenType(dao.getProjectId(),
                                    Integer.valueOf(dao.getCurrentRound()), TokenTypeEnum.SWAP.getType());
                    if (tokenReceivedRecords == null || tokenReceivedRecords.size() == 0) {
                        daoIncomeVo
                                .setDaoAssetPool(dao.getDaoAssetPool() == null ? BigDecimal.ZERO : dao.getDaoAssetPool());
                        daoIncomeVo.setUnchangedTokenAmount(
                                dao.getUnchangedTokenAmount() == null ? BigDecimal.ZERO : dao.getUnchangedTokenAmount());
                    } else {
                        BigDecimal daoAssetPool = tokenReceivedRecords.stream().map(TokenReceivedRecord::getEthAmount)
                                .reduce(BigDecimal.ZERO, BigDecimal::add);
                        BigDecimal unchangedTokenAmount =
                                tokenReceivedRecords.stream().map(v -> new BigDecimal(String.valueOf(v.getTokenNum())))
                                        .reduce(BigDecimal.ZERO, BigDecimal::add);
                        if (dao.getDaoAssetPool() != null) {
                            daoIncomeVo.setDaoAssetPool(dao.getDaoAssetPool().subtract(daoAssetPool));
                        }
                        if (dao.getUnchangedTokenAmount() != null) {
                            daoIncomeVo
                                    .setUnchangedTokenAmount(dao.getUnchangedTokenAmount().subtract(unchangedTokenAmount));

                        }
                    }
                }

                daoIncomeVo.setErc20Collected(new BigDecimal(String.valueOf(dao.getReceivedToken()))
                        .add(new BigDecimal(String.valueOf(dao.getSwapToken() == null ? 0 : dao.getSwapToken())))
                        .add(new BigDecimal(String.valueOf(dao.getTransferToken() == null ? 0 : dao.getTransferToken()))));
                daoIncomeVo.setEthCollected(new BigDecimal(String.valueOf(dao.getReceivedEth())).stripTrailingZeros().toPlainString());
                if (!ProtoDaoConstant.D4APause && !dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                    daoIncomeVo.setErc20Collectable(
                            new BigDecimal(String.valueOf(dao.getUnclaimedToken() == null ? 0 : dao.getUnclaimedToken())));
                    daoIncomeVo.setEthCollectable(
                            new BigDecimal(String.valueOf(dao.getUnclaimedEth() == null ? 0 : dao.getUnclaimedEth())).stripTrailingZeros().toPlainString());
                    BigDecimal unclaimedToken =
                            dao.getUnclaimedToken() == null ? BigDecimal.ZERO : dao.getUnclaimedToken();
                    daoIncomeVo.setErc20CollectableEth(transferEthForDao(dao, new BigDecimal(String.valueOf(unclaimedToken)), userAddress));

                    if (unclaimedToken.compareTo(BigDecimal.ZERO) > 0
                            && daoIncomeVo.getErc20CollectableEth().compareTo(BigDecimal.ZERO) > 0) {
                        daoIncomeVo.setSwapEthRatio(BigDecimal.ONE.divide(unclaimedToken, 18, RoundingMode.FLOOR)
                                .multiply(daoIncomeVo.getErc20CollectableEth()).stripTrailingZeros());
                    }
                }

                Erc20Collected erc20Collected = new Erc20Collected();
                erc20Collected.setCollectedName("DAO Creator Reward");
                erc20Collected.setDaoNumber(dao.getDaoNumber() + "");
                erc20Collected.setCollectedAmount(new BigDecimal(String.valueOf(dao.getReceivedToken()))
                        .add(dao.getSwapToken() == null ? BigDecimal.ZERO : dao.getSwapToken())
                        .add(dao.getTransferToken() == null ? BigDecimal.ZERO : dao.getTransferToken()));
                erc20CollectedList.add(erc20Collected);

                Erc20Collectable erc20Collectable = new Erc20Collectable();
                erc20Collectable.setCollectedName("DAO Creator Reward");
                erc20Collectable.setDaoNumber(dao.getDaoNumber() + "");
                erc20Collectable.setCollectedAmount(
                        new BigDecimal(String.valueOf(dao.getUnclaimedToken() == null ? 0 : dao.getUnclaimedToken())));
                if (!ProtoDaoConstant.D4APause && !dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                        && erc20Collectable.getCollectedAmount().compareTo(BigDecimal.ZERO) > 0) {
                    daoIncomeVo.getProjectIds().add(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                }
                erc20Collectable.setIsPaused(dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
                if (dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                    erc20Collectable.setPauseedMsg(dao.getDaoName() + " (D4A@" + dao.getDaoNumber()
                            + ") This function is suspended for security reasons.");
                }
                if (ProtoDaoConstant.D4APause) {
                    erc20Collectable.setIsPaused(1);
                    erc20Collectable.setPauseedMsg("D4A This function is suspended for security reasons.");
                }
                erc20CollectableList.add(erc20Collectable);

                IDaoDrbStatisticsService daoDrbStatisticsService =
                        SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
                if (daoDrbStatisticsService != null) {
                    DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
                    if (daoDrbStatistics != null) {

                        if (dao.getSwapEth() != null) {
                            daoIncomeVo.setErc20Revenue(dao.getSwapEth());
                        }
                        if (dao.getReceivedToken() != null) {
                            daoIncomeVo.setErc20Balance(new BigDecimal(String.valueOf(dao.getReceivedToken())));
                            daoIncomeVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                        }
                        daoIncomeVo.setErc20BalanceEth(transferEthForContract(dao,
                                new BigDecimal(String.valueOf(dao.getReceivedToken())), userAddress));

                        if (daoIncomeVo.getSwapEthRatio().equals(BigDecimal.ZERO)
                                && daoIncomeVo.getErc20Balance().compareTo(BigDecimal.ZERO) > 0
                                && daoIncomeVo.getErc20BalanceEth().compareTo(BigDecimal.ZERO) > 0) {
                            daoIncomeVo.setSwapEthRatio(
                                    BigDecimal.ONE.divide(daoIncomeVo.getErc20Balance(), 18, RoundingMode.FLOOR)
                                            .multiply(daoIncomeVo.getErc20BalanceEth()).stripTrailingZeros());
                        }
                    }

                }
            }

            if (o instanceof Canvas) {
                Canvas canvas = (Canvas) o;
                Dao dao = null;
                IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
                if (daoIncomeVo.getDaoNumber() == null) {
                    if (daoService != null) {
                        dao = daoService.getById(canvas.getDaoId());
                        if (dao != null) {
                            //钱包这里topup模式的不展示
                            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
                                continue;
                            }
                            daoIncomeVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));
                            daoIncomeVo.setDaoName(dao.getDaoName());
                            daoIncomeVo.setDaoVersion(dao.getDaoVersion());
                            daoIncomeVo
                                    .setIsPaused(dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
                            if (dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                                daoIncomeVo.setPauseedMsg(dao.getDaoName() + " (D4A@" + dao.getDaoNumber()
                                        + ") This function is suspended for security reasons.");
                            }
                            // if(dao.getUnchangedTokenAmount() != null && dao.getDaoAssetPool() != null){
                            // daoIncomeVo.setSwapEthRatio(BigDecimal.ONE.divide(dao.getUnchangedTokenAmount(), 18,
                            // RoundingMode.FLOOR).multiply(dao.getDaoAssetPool()).stripTrailingZeros().toPlainString());
                            // }
                            // 查询当天兑换的记录
                            ITokenReceivedRecordService tokenReceivedRecordService =
                                    SpringBeanUtil.getBean(ITokenReceivedRecordService.class);
                            if (tokenReceivedRecordService != null) {
                                List<TokenReceivedRecord> tokenReceivedRecords =
                                        tokenReceivedRecordService.recordListByTokenType(dao.getProjectId(),
                                                Integer.valueOf(dao.getCurrentRound()), TokenTypeEnum.SWAP.getType());
                                if (tokenReceivedRecords == null || tokenReceivedRecords.size() == 0) {
                                    daoIncomeVo.setDaoAssetPool(
                                            dao.getDaoAssetPool() == null ? BigDecimal.ZERO : dao.getDaoAssetPool());
                                    daoIncomeVo.setUnchangedTokenAmount(dao.getUnchangedTokenAmount() == null
                                            ? BigDecimal.ZERO : dao.getUnchangedTokenAmount());
                                } else {
                                    BigDecimal daoAssetPool =
                                            tokenReceivedRecords.stream().map(TokenReceivedRecord::getEthAmount)
                                                    .reduce(BigDecimal.ZERO, BigDecimal::add);
                                    BigDecimal unchangedTokenAmount = tokenReceivedRecords.stream()
                                            .map(v -> new BigDecimal(String.valueOf(v.getTokenNum())))
                                            .reduce(BigDecimal.ZERO, BigDecimal::add);
                                    daoIncomeVo.setDaoAssetPool(dao.getDaoAssetPool().subtract(daoAssetPool));
                                    daoIncomeVo.setUnchangedTokenAmount(
                                            dao.getUnchangedTokenAmount().subtract(unchangedTokenAmount));
                                }
                            }
                            daoIncomeVo.setPayCurrencyType(dao.getPayCurrencyType());
                            daoIncomeVo.setInputTokenAddress(dao.getInputTokenAddress());
                            daoIncomeVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
                            daoIncomeVo.setInputTokenDecimals(dao.getInputTokenDecimals());
                        }
                    }
                    daoIncomeVo.setDaoNumber(canvas.getDaoNumber());
                    daoIncomeVo.setDaoSymbol(canvas.getDaoSymbol());
                } else {
                    if (daoService != null) {
                        dao = daoService.getById(canvas.getDaoId());
                        //钱包这里topup模式的不展示
                        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
                            continue;
                        }
                        daoIncomeVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));
                    }
                }
                ICanvasDrbStatisticsService canvasDrbStatisticsService =
                        SpringBeanUtil.getBean(ICanvasDrbStatisticsService.class);
                if (!canvas.getIsMinter() && canvasDrbStatisticsService != null) {
                    CanvasDrbStatistics canvasDrbStatistics =
                            canvasDrbStatisticsService.selectLastedByCanvasId(canvas.getId());
                    if (canvasDrbStatistics != null) {
                        BigDecimal mintRewardValue =
                                daoIncomeVo.getMintReward() == null ? BigDecimal.ZERO : daoIncomeVo.getMintReward();
                        daoIncomeVo.setMintReward(mintRewardValue.add(canvasDrbStatistics.getMintRevenueExTax()));
                        MintReward mintReward = new MintReward();
                        mintReward.setCanvasName(canvas.getCanvasName() + "(D4A@" + canvas.getDaoNumber() + "/Canvas*"
                                + canvas.getCanvasNumber() + ")");
                        if (canvasDrbStatistics.getMintRevenueExTax() != null) {
                            mintReward.setEthAmount(canvasDrbStatistics.getMintRevenueExTax());
                        }
                        mintRewardList.add(mintReward);
                    }
                } else {
                    BigDecimal mintRewardValue =
                            daoIncomeVo.getMintReward() == null ? BigDecimal.ZERO : daoIncomeVo.getMintReward();
                    daoIncomeVo.setMintReward(mintRewardValue);
                }

                BigDecimal erc20Revenue =
                        daoIncomeVo.getErc20Revenue() == null ? BigDecimal.ZERO : daoIncomeVo.getErc20Revenue();
                daoIncomeVo.setErc20Revenue(
                        erc20Revenue.add(canvas.getSwapEth() == null ? BigDecimal.ZERO : canvas.getSwapEth()));

                BigDecimal erc20CollectedValue =
                        daoIncomeVo.getErc20Collected() == null ? BigDecimal.ZERO : daoIncomeVo.getErc20Collected();
                BigDecimal ethCollectedValue =
                        daoIncomeVo.getEthCollected() == null ? BigDecimal.ZERO : new BigDecimal(daoIncomeVo.getEthCollected());
                daoIncomeVo.setErc20Collected(
                        erc20CollectedValue.add(new BigDecimal(String.valueOf(canvas.getReceivedToken())))
                                .add(new BigDecimal(String.valueOf(canvas.getSwapToken() == null ? 0 : canvas.getSwapToken())))
                                .add(new BigDecimal(
                                        String.valueOf(canvas.getTransferToken() == null ? 0 : canvas.getTransferToken()))));
                daoIncomeVo.setEthCollected(
                        ethCollectedValue.add(new BigDecimal(String.valueOf(canvas.getReceivedEth()))).stripTrailingZeros().toPlainString());

                if (!ProtoDaoConstant.D4APause && !canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                        && !canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
                    BigDecimal erc20CollectableValue =
                            daoIncomeVo.getErc20Collectable() == null ? BigDecimal.ZERO : daoIncomeVo.getErc20Collectable();
                    BigDecimal ethCollectableValue =
                            daoIncomeVo.getEthCollectable() == null ? BigDecimal.ZERO : new BigDecimal(daoIncomeVo.getEthCollectable());
                    daoIncomeVo.setErc20Collectable(erc20CollectableValue.add(new BigDecimal(
                            String.valueOf(canvas.getUnclaimedToken() == null ? 0 : canvas.getUnclaimedToken()))));
                    daoIncomeVo.setEthCollectable(ethCollectableValue.add(new BigDecimal(
                            String.valueOf(canvas.getUnclaimedEth() == null ? 0 : canvas.getUnclaimedEth()))).stripTrailingZeros().toPlainString());
                    daoIncomeVo.setErc20CollectableEth(daoIncomeVo.getErc20CollectableEth()
                            .add(transferEthForCanvas(canvas, new BigDecimal(String.valueOf(canvas.getUnclaimedToken() == null ? 0 : canvas.getUnclaimedToken())),
                                    userAddress)));
                    if (daoIncomeVo.getSwapEthRatio().equals(BigDecimal.ZERO)
                            && daoIncomeVo.getErc20Collectable().compareTo(BigDecimal.ZERO) > 0
                            && daoIncomeVo.getErc20CollectableEth().compareTo(BigDecimal.ZERO) > 0) {
                        daoIncomeVo.setSwapEthRatio(
                                BigDecimal.ONE.divide(daoIncomeVo.getErc20Collectable(), 18, RoundingMode.FLOOR)
                                        .multiply(daoIncomeVo.getErc20CollectableEth()).stripTrailingZeros());
                    }

                    if (daoIncomeVo.getErc20Collectable().compareTo(BigDecimal.ZERO) > 0) {
                        if (!canvas.getIsMinter()) {
                            daoIncomeVo.getCanvasIds().add(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
                        } else {
                            daoIncomeVo.getProjectIds().add(CommonUtil.addHexPrefixIfNotExist(canvas.getProjectId()));
                        }
                    }
                }

                if (canvas.getIsMinter()) {
                    Erc20Collected erc20Collected = new Erc20Collected();
                    erc20Collected.setCollectedName("DAO Minter Reward");
                    erc20Collected.setDaoNumber(canvas.getDaoNumber() + "");
                    erc20Collected.setCanvasNumber(canvas.getCanvasNumber() + "");
                    erc20Collected.setCollectedAmount(new BigDecimal(String.valueOf(canvas.getReceivedToken()))
                            .add(canvas.getSwapToken() == null ? BigDecimal.ZERO : canvas.getSwapToken())
                            .add(canvas.getTransferToken() == null ? BigDecimal.ZERO : canvas.getTransferToken()));
                    erc20CollectedList.add(erc20Collected);

                    Erc20Collectable erc20Collectable = new Erc20Collectable();
                    erc20Collectable.setCollectedName("DAO Minter Reward");
                    erc20Collectable.setDaoNumber(canvas.getDaoNumber() + "");
                    erc20Collectable.setCanvasNumber(canvas.getCanvasNumber() + "");
                    erc20Collectable.setCollectedAmount(new BigDecimal(
                            String.valueOf(canvas.getUnclaimedToken() == null ? 0 : canvas.getUnclaimedToken())));
                    erc20Collectable
                            .setIsPaused(canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
                    if (canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                        erc20Collectable.setPauseedMsg(dao.getDaoName() + "  (D4A@" + dao.getDaoNumber()
                                + ") This function is suspended for security reasons.");
                    }
                    if (ProtoDaoConstant.D4APause) {
                        erc20Collectable.setIsPaused(1);
                        erc20Collectable.setPauseedMsg("D4A This function is suspended for security reasons.");
                    }
                    erc20CollectableList.add(erc20Collectable);
                } else {
                    Erc20Collected erc20Collected = new Erc20Collected();
                    erc20Collected.setCollectedName(canvas.getCanvasName() + "(D4A@" + canvas.getDaoNumber()
                            + "/Canvas*" + canvas.getCanvasNumber() + ")");
                    erc20Collected.setDaoNumber(canvas.getDaoNumber() + "");
                    erc20Collected.setCanvasNumber(canvas.getCanvasNumber() + "");
                    erc20Collected.setCollectedAmount(new BigDecimal(String.valueOf(canvas.getReceivedToken()))
                            .add(canvas.getSwapToken() == null ? BigDecimal.ZERO : canvas.getSwapToken())
                            .add(canvas.getTransferToken() == null ? BigDecimal.ZERO : canvas.getTransferToken()));
                    erc20CollectedList.add(erc20Collected);

                    Erc20Collectable erc20Collectable = new Erc20Collectable();
                    erc20Collectable.setCollectedName(canvas.getCanvasName() + "(D4A@" + canvas.getDaoNumber()
                            + "/Canvas*" + canvas.getCanvasNumber() + ")");
                    erc20Collectable.setDaoNumber(canvas.getDaoNumber() + "");
                    erc20Collectable.setCanvasNumber(canvas.getCanvasNumber() + "");
                    erc20Collectable.setCollectedAmount(new BigDecimal(
                            String.valueOf(canvas.getUnclaimedToken() == null ? 0 : canvas.getUnclaimedToken())));
                    erc20Collectable.setIsPaused(canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())
                            || canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus()) ? 1 : 0);
                    if (canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
                        erc20Collectable
                                .setPauseedMsg(canvas.getCanvasName() + " (D4A@" + canvas.getDaoNumber() + "/Canvas*"
                                        + canvas.getCanvasNumber() + ") This function is suspended for security reasons.");
                    }
                    if (canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                        erc20Collectable.setPauseedMsg(dao.getDaoName() + "  (D4A@" + dao.getDaoNumber()
                                + ") This function is suspended for security reasons.");
                    }
                    if (ProtoDaoConstant.D4APause) {
                        erc20Collectable.setIsPaused(1);
                        erc20Collectable.setPauseedMsg("D4A This function is suspended for security reasons.");
                    }
                    erc20CollectableList.add(erc20Collectable);
                }

                BigDecimal erc20Balance =
                        daoIncomeVo.getErc20Balance() == null ? BigDecimal.ZERO : daoIncomeVo.getErc20Balance();
                daoIncomeVo
                        .setErc20Balance(erc20Balance.add(new BigDecimal(String.valueOf(canvas.getReceivedToken()))));
                daoIncomeVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(canvas.getProjectId()));
                if (dao != null) {
                    log.info("[DaoIncomeVo]daoId:{} erc20Balance:{}", dao.getId(), daoIncomeVo.getErc20Balance());
                } else {
                    log.warn("[DaoIncomeVo]dao is null daoId:{} erc20Balance:{}", canvas.getDaoId(), daoIncomeVo.getErc20Balance());
                }
                daoIncomeVo.setErc20BalanceEth(transferEthForContract(dao,
                        erc20Balance.add(new BigDecimal(String.valueOf(canvas.getReceivedToken()))), userAddress));
                if (daoIncomeVo.getSwapEthRatio().equals(BigDecimal.ZERO)
                        && daoIncomeVo.getErc20Balance().compareTo(BigDecimal.ZERO) > 0
                        && daoIncomeVo.getErc20BalanceEth().compareTo(BigDecimal.ZERO) > 0) {
                    daoIncomeVo
                            .setSwapEthRatio(BigDecimal.ONE.divide(daoIncomeVo.getErc20Balance(), 18, RoundingMode.FLOOR)
                                    .multiply(daoIncomeVo.getErc20BalanceEth()).stripTrailingZeros());
                }
            }

        }

        // if(daoIncomeVo.getMintReward() != null && daoIncomeVo.getMintReward().compareTo(BigDecimal.ZERO) > 0){
        // daoIncomeVo.setMintReward(daoIncomeVo.getMintReward()
        // .multiply(BigDecimal.ONE.subtract(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))
        // .add(new BigDecimal(Dao4ArtConstant.MINT_D4A_FEE_RATIO).divide(new
        // BigDecimal(Dao4ArtConstant.RATIO_BASE))))));
        // }

        daoIncomeVo.setMintRewardList(mintRewardList);
        daoIncomeVo.setErc20CollectedList(erc20CollectedList);
        daoIncomeVo.setErc20CollectableList(erc20CollectableList);

        return daoIncomeVo;
    }

    public static BigDecimal transferEth(Dao dao, BigDecimal tokenAmount) {

        if (dao == null || tokenAmount == null) {
            return BigDecimal.ZERO;
        }
        if (dao.getUnchangedTokenAmount() == null || dao.getDaoAssetPool() == null) {
            ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
            IDaoDrbStatisticsService daoDrbStatisticsService = SpringBeanUtil.getBean(IDaoDrbStatisticsService.class);
            if (canvasService != null && daoDrbStatisticsService != null) {
                List<Canvas> canvasList = canvasService.listCanvasByDaoId(dao.getId() + "");
                BigDecimal canvsSwopToken = canvasList.stream().filter(v -> v.getSwapToken() != null)
                        .map(Canvas::getSwapToken).reduce(BigDecimal.ZERO, BigDecimal::add);
                DaoDrbStatistics daoDrbStatistics = daoDrbStatisticsService.selectLastedDrbByDaoId(dao.getId());
                if (daoDrbStatistics != null) {
                    if (daoDrbStatistics.getDaoReward() == null) {
                        daoDrbStatistics = daoDrbStatisticsService.selectByDaoIdAndDrbNumber(dao.getId(),
                                daoDrbStatistics.getDrbNumber() - 1);
                    }
                    if (daoDrbStatistics.getDaoReward() != null && daoDrbStatistics.getDaoReward().compareTo(BigDecimal.ZERO) > 0) {
                        return tokenAmount
                                .divide(daoDrbStatistics.getDaoReward()
                                        .subtract(
                                                new BigDecimal(String.valueOf(dao.getSwapToken() == null ? 0 : dao.getSwapToken())))
                                        .subtract(canvsSwopToken), 18, BigDecimal.ROUND_FLOOR)
                                .multiply(daoDrbStatistics.getDaoAssetPool());
                    }

                }
            }
            return BigDecimal.ZERO;
        }

        return tokenAmount.divide(dao.getUnchangedTokenAmount(), 18, BigDecimal.ROUND_FLOOR)
                .multiply(dao.getDaoAssetPool());
    }

    /**
     * function exchangeERC20ToETH(bytes32 _project_id, uint256 amount, address _to) public nonReentrant
     * returns(uint256){
     *
     * @param dao
     * @param tokenAmount
     * @param userAddress
     * @return
     */
    public static BigDecimal transferEthForContract(Dao dao, BigDecimal tokenAmount, String userAddress) {

        if (dao == null || tokenAmount == null || tokenAmount.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        if (ProtoDaoConstant.D4APause || dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
            return BigDecimal.ZERO;
        }
        log.info("[transferEthForContract] daoId:{} tokenAmount:{}", dao.getId(), tokenAmount);
        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ProtoDaoConstant.protocolContract);
            if (StringUtils.isBlank(userAddress)) {
                userAddress = dao.getOwnerAddress();
            }
            infuraCallRequestDto.setFrom(userAddress);
            String token = decToHex(
                    tokenAmount.multiply(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())).stripTrailingZeros().toPlainString());
            String data = ProtoDaoConstant.exchangeERC20ToETH
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(dao.getProjectId()))
                    + CommonUtil.fillLeadingZerosInBytes32(token)
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(userAddress));

            log.info("[transferEthForContract] data:{}", data);
            infuraCallRequestDto.setData(data);

            ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
            if (iSubscriptionService != null) {
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    if (StringUtils.isNotBlank(result.getResultDesc())
                            && result.getResultDesc().contains("burn amount exceeds balance")) {
                        log.warn("[transferEthForContract] error result:{} ", result.getResultDesc());
                        return BigDecimal.ZERO;
                    } else {
                        log.error("[transferEthForContract] error result:{} ", result.getResultDesc());
                    }
                    throw new RuntimeException(result.getResultDesc());
                }
                log.info("[transferEthForContract]infura return data:{}", result.getData());
                String canvasInfoData = result.getData();
                String price = CommonUtil.hexToTenString(canvasInfoData);

                return new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18,
                        RoundingMode.FLOOR);
            }
        } catch (Exception e) {
            log.info("[transferEthForContract] daoId:{} tokenAmount:{} e:", dao.getId(), tokenAmount, e);
        }
        return BigDecimal.ZERO;

    }

    /**
     * function getTokenToETH(bytes32 projectId, uint256 tokenAmount) public view returns (uint256 ethAmount)
     *
     * @param projectId   projectId
     * @param tokenAmount tokenAmount
     * @return BigDecimal
     */
    @Deprecated
    public static BigDecimal getTokenToETH(String projectId, BigDecimal tokenAmount) {

        log.info("[getTokenToETH] projectId:{} tokenAmount:{}", projectId, tokenAmount);
        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ProtoDaoConstant.protocolContract);

            String token = decToHex(
                    tokenAmount.multiply(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)).stripTrailingZeros().toPlainString());
            String data = ProtoDaoConstant.getTokenToETH
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(projectId))
                    + CommonUtil.fillLeadingZerosInBytes32(token);

            log.info("[getTokenToETH] data:{}", data);
            infuraCallRequestDto.setData(data);

            ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
            if (iSubscriptionService != null) {
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                    if (StringUtils.isNotBlank(result.getResultDesc())
                            && result.getResultDesc().contains("burn amount exceeds balance")) {
                        log.warn("[getTokenToETH] error result:{} ", result.getResultDesc());
                        return BigDecimal.ZERO;
                    } else {
                        log.error("[getTokenToETH] error result:{} ", result.getResultDesc());
                    }
                    throw new RuntimeException(result.getResultDesc());
                }
                log.info("[getTokenToETH]infura return data:{}", result.getData());
                String canvasInfoData = result.getData();
                String price = CommonUtil.hexToTenString(canvasInfoData);

                return new BigDecimal(price).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO), 18,
                        RoundingMode.FLOOR);
            }
        } catch (Exception e) {
            log.error("[getTokenToETH] projectId:{} tokenAmount:{} e:", projectId, tokenAmount, e);
        }
        return BigDecimal.ZERO;

    }

    /**
     * claimProjectERC20RewardWithETH(bytes32 _project_id){
     *
     * @param dao
     * @param tokenAmount
     * @param userAddress
     * @return
     */
    public static BigDecimal transferEthForDao(Dao dao, BigDecimal tokenAmount, String userAddress) {

        if (dao == null || tokenAmount == null || tokenAmount.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        if (ProtoDaoConstant.D4APause || dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
            return BigDecimal.ZERO;
        }
        log.info("[transferEthForDao] daoId:{} tokenAmount:{} userAddress:{}", dao.getId(), tokenAmount, userAddress);
        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ProtoDaoConstant.protocolContract);
            if (StringUtils.isBlank(userAddress)) {
                userAddress = dao.getOwnerAddress();
            }
            if (dao.getDaoVersion().equals(3)) {
                String multicallData = UserWalletDaoIncomeVo.buildClaimDaoRewardMulticall(dao, tokenAmount, userAddress);
                return UserWalletDaoIncomeVo.multicall(multicallData, userAddress, dao.getInputTokenDecimals());
            }

            infuraCallRequestDto.setFrom(userAddress);
            String data = ProtoDaoConstant.claimProjectERC20RewardWithETH
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(dao.getProjectId()));

            log.info("[transferEthForDao] data:{}", data);
            infuraCallRequestDto.setData(data);

            ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
            if (iSubscriptionService != null) {
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                    if ("execution reverted".equals(result.getResultDesc())) {
//                        log.warn("[transferEthForDao] error result:{} ", result.getResultDesc());
//                        return BigDecimal.ZERO;
//                    }
                    log.error("[transferEthForDao] error result:{} ", result.getResultDesc());
                    throw new RuntimeException(result.getResultDesc());
                }
                log.info("[transferEthForDao]infura return data:{}", result.getData());
                String canvasInfoData = result.getData();
                String price = CommonUtil.hexToTenString(canvasInfoData);

                return new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18,
                        RoundingMode.FLOOR);
            }
        } catch (Exception e) {
            log.info("[transferEthForDao] daoId:{} tokenAmount:{} e:", dao.getId(), tokenAmount, e);
        }
        return BigDecimal.ZERO;

    }

    /**
     * claimCanvasRewardWithETH(bytes32 _project_id){
     *
     * @param canvas
     * @param tokenAmount
     * @param userAddress
     * @return
     */
    public static BigDecimal transferEthForCanvas(Canvas canvas, BigDecimal tokenAmount, String userAddress) {

        if (canvas == null || tokenAmount == null || tokenAmount.compareTo(BigDecimal.ZERO) <= 0) {
            return BigDecimal.ZERO;
        }
        if (ProtoDaoConstant.D4APause || canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                || canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())) {
            return BigDecimal.ZERO;
        }
        log.info("[transferEthForCanvas] canvasId:{} tokenAmount:{} userAddress:{}", canvas.getId(), tokenAmount, userAddress);
        try {
            IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
            Dao dao = daoService.getById(canvas.getDaoId());
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ProtoDaoConstant.protocolContract);
            if (StringUtils.isBlank(userAddress)) {
                userAddress = canvas.getOwnerAddress();
            }
            if (dao.getDaoVersion().equals(3)) {
                String multicallData = UserWalletDaoIncomeVo.buildClaimCanvasRewardMulticall(canvas, dao, tokenAmount, userAddress);
                return UserWalletDaoIncomeVo.multicall(multicallData, userAddress, dao.getInputTokenDecimals());
            }

            infuraCallRequestDto.setFrom(userAddress);
            String data = ProtoDaoConstant.claimCanvasRewardWithETH
                    + CommonUtil.fillLeadingZerosInBytes32(CommonUtil.removeHexPrefixIfExists(canvas.getCanvasId()));

            log.info("[transferEthForCanvas] data:{}", data);
            infuraCallRequestDto.setData(data);

            ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
            if (iSubscriptionService != null) {
                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                    if ("execution reverted".equals(result.getResultDesc())) {
//                        log.warn("[transferEthForCanvas] error result:{} ", result.getResultDesc());
//                        return BigDecimal.ZERO;
//                    }
                    log.error("[transferEthForCanvas] error result:{} ", result.getResultDesc());
                    throw new RuntimeException(result.getResultDesc());
                }
                log.info("[transferEthForCanvas]infura return data:{}", result.getData());
                String canvasInfoData = result.getData();
                String price = CommonUtil.hexToTenString(canvasInfoData);

                return new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals()), 18,
                        RoundingMode.FLOOR);
            }
        } catch (Exception e) {
            log.info("[transferEthForCanvas] daoId:{} tokenAmount:{} e:", canvas.getId(), tokenAmount, e);
        }
        return BigDecimal.ZERO;

    }

    /**
     * 十进制数据转换为十六进制字符串数
     *
     * @param dec
     * @return
     */
    public static String decToHex(String dec) {
        BigDecimal sixteen = new BigDecimal(16);
        BigDecimal n = new BigDecimal(dec);
        StringBuffer sb = new StringBuffer(8);
        String a;
        char[] b = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
        /** 判断是否为0 */
        if (n.compareTo(BigDecimal.ZERO) == 0) {
            return n.toString();
        }
        while (n.compareTo(BigDecimal.ZERO) != 0) {
            /** BigDecimal除法取余数 */
            sb = sb.append(b[n.remainder(sixteen).intValue()]);
            /** BigDecimal除法,省略小数点后的位数且不进位 */
            n = n.divide(sixteen, 0, BigDecimal.ROUND_DOWN);
        }
        a = sb.reverse().toString();
        return a;
    }

    public static String buildClaimCanvasRewardMulticall(Canvas canvas, Dao dao, BigDecimal amount, String address) throws Exception {
        Type[] args = new Type[1];
        args[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
        String data = EthTransactionUtil.encodeFunction("claimCanvasRewardFunding", args);
        log.info("[buildClaimCanvasRewardMulticall] data:{}", data);
        Type[] args1 = new Type[3];
        args1[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        args1[1] = TypeDecoder.instantiateType("uint256", amount.multiply(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
        args1[2] = TypeDecoder.instantiateType("address", address);
        String data1 = EthTransactionUtil.encodeFunction("exchangeOutputToInput", args1); // exchangeERC20ToETH
        log.info("[buildClaimCanvasRewardMulticall] data1:{}", data1);
        Type[] args2 = new Type[1];
        List<String> params = new ArrayList<>();
        params.add(data);
        params.add(data1);
        args2[0] = TypeDecoder.instantiateType("bytes[]", params);
        String data2 = EthTransactionUtil.encodeFunction("multicall", args2);

        return data2;

    }

    public static String buildClaimDaoRewardMulticall(Dao dao, BigDecimal amount, String address) throws Exception {
        Type[] args = new Type[1];
        args[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        String data = EthTransactionUtil.encodeFunction("claimDaoNftOwnerReward", args);
        log.info("[buildClaimDaoRewardMulticall] data:{}", data);

        Type[] args1 = new Type[3];
        args1[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        args1[1] = TypeDecoder.instantiateType("uint256", amount.multiply(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
        args1[2] = TypeDecoder.instantiateType("address", address);
        String data1 = EthTransactionUtil.encodeFunction("exchangeOutputToInput", args1);  // exchangeERC20ToETH

        log.info("[buildClaimDaoRewardMulticall] data1:{}", data1);

        Type[] args2 = new Type[1];
        List<String> params = new ArrayList<>();
        params.add(data);
        params.add(data1);
        args2[0] = TypeDecoder.instantiateType("bytes[]", params);
        String data2 = EthTransactionUtil.encodeFunction("multicall", args2);

        return data2;

    }

    /**
     * multicall 0xac9650d8 只有dao_version = 3 的调用这个方法
     *
     * @param
     * @return BigDecimal
     */
    public static BigDecimal multicall(String data, String userAddress, Integer inputTokenDecimal) {

        log.info("[multicall] data:{}  userAddress:{}", data, userAddress);
        try {
            InfuraCallRequestDto infuraCallRequestDto = new InfuraCallRequestDto();
            infuraCallRequestDto.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto.setTo(ProtoDaoConstant.protocolContract);
            infuraCallRequestDto.setFrom(userAddress);
            log.info("[multicall] data:{}", data);
            infuraCallRequestDto.setData(data);
// 此处逻辑不用，合约方法已不维护
//            ISubscriptionService iSubscriptionService = SpringBeanUtil.getBean(ISubscriptionService.class);
//            iSubscriptionService = null;
//            if (iSubscriptionService != null) {
//                Result<String> result = iSubscriptionService.infuraCall(infuraCallRequestDto);
//                if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
//                    log.error("[multicall] error resultCode:{} result:{} ", result.getResultCode(), result.getResultDesc());
//                    throw new RuntimeException(result.getResultDesc());
//                }
//                log.info("[multicall]infura return data:{}", result.getData());
//                String canvasInfoData = result.getData();
//                canvasInfoData = CommonUtil.removeHexPrefixIfExists(canvasInfoData);
//                List<String> dataList = CommonUtil.splitBy32Bytes(canvasInfoData);
//
//                String price = CommonUtil.hexToTenString(dataList.get(7));
//
//                return new BigDecimal(price).divide(CommonUtil.getPowBigDecimal(inputTokenDecimal), 18,
//                        RoundingMode.FLOOR);
//            }
        } catch (Exception e) {
            log.info("[multicall] data:{}  userAddress:{} e:", data, userAddress, e);
        }
        return BigDecimal.ZERO;

    }

    public static void main(String[] arg) throws Exception {
        // String str = "2000000";
        // str = decToHex(str);
        // System.out.println(str);
        // DaoIncomeVo daoIncomeVo = new DaoIncomeVo();
        // daoIncomeVo.setMintReward(new BigDecimal("0.2"));
        // BigDecimal mintRewardValue =
        // daoIncomeVo.getMintReward() == null ? BigDecimal.ZERO : daoIncomeVo.getMintReward();
        // CanvasDrbStatistics canvasDrbStatistics = new CanvasDrbStatistics();
        // canvasDrbStatistics.setMintRevenue(new BigDecimal("0.5"));
        // System.out.println(mintRewardValue.add(canvasDrbStatistics.getMintRevenue())
        // .multiply(BigDecimal.ONE.subtract(new BigDecimal(Dao4ArtConstant.MINT_PROJECT_FEE_RATIO)
        // .divide(new BigDecimal(Dao4ArtConstant.RATIO_BASE))
        // .add(new BigDecimal(Dao4ArtConstant.MINT_D4A_FEE_RATIO)
        // .divide(new BigDecimal(Dao4ArtConstant.RATIO_BASE))))));

//        String aa = "execution reverted: ERC20: burn amount exceeds balance";
//        System.out.println(aa.contains("burn amount exceeds balance"));
        //-32000合约里是方法找不到的意思
//        Type[] types = new Type[1];
//        types[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist("eaf74444ff6cd0e4476a497805f8219bfd401d1c82aed0e6066548584a9a33c3"));
//        String data = EthTransactionUtil.encodeFunction("claimCanvasReward", types);
//        System.out.println(data);

        Type[] args = new Type[1];
        args[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist("eaf74444ff6cd0e4476a497805f8219bfd401d1c82aed0e6066548584a9a33c3"));
        String data = EthTransactionUtil.encodeFunction("claimCanvasReward", args);
        System.out.println(data);

        Type[] args1 = new Type[3];
        args1[0] = TypeDecoder.instantiateType("bytes32", CommonUtil.addHexPrefixIfNotExist("e3aa4dadfaf4fbd68621e09149b449daac5ddfa5016a2208290c326888333cce"));
        args1[1] = TypeDecoder.instantiateType("uint256", new BigDecimal("37399421.959190096009868902").multiply(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
        args1[2] = TypeDecoder.instantiateType("address", "0xb90b90225e628188a16c1ab2ffbd8372e49b39df");
        String data1 = EthTransactionUtil.encodeFunction("exchangeERC20ToETH", args1);
        System.out.println(data1);

        Type[] args2 = new Type[1];
        List<String> params = new ArrayList<>();
        params.add(data);
        params.add(data1);
        args2[0] = TypeDecoder.instantiateType("bytes[]", params);
        String data2 = EthTransactionUtil.encodeFunction("multicall", args2);
        System.out.println(data2);
    }

    /**
     * dao收益记录
     */
    @Data
    public static class DaoIncomeVo {

        /**
         * dao 的projectId erc20Balance swap时用
         */
        private String projectId;

        /**
         * Dao名称
         */
        private String daoName;

        /**
         * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
         */
        private Integer daoVersion;

        /**
         * Dao编号
         */
        private Integer daoNumber;

        /**
         * dao symbol
         */
        private String daoSymbol;

        /**
         * 是否停机 0-未停机 1-已停机
         */
        private Integer isPaused;

        /**
         * 停机提示信息
         */
        private String pauseedMsg;

        /**
         * DAO的projectId collect或swap时用
         */
        private List<String> projectIds = new ArrayList<>();

        /**
         * Dao下Canvas的canvasId collect或swap时用
         */
        private List<String> canvasIds = new ArrayList<>();

        /**
         * 铸造eth收入
         */
        private BigDecimal mintReward = BigDecimal.ZERO;

        /**
         * 铸造token收入
         */
        private BigDecimal mintRewardToken = BigDecimal.ZERO;

        /**
         * 1.4 是否开启Erc20支付模式 false-否 true-是
         */
        private Boolean erc20PaymentMode = false;

        /**
         * Dao下erc20收益
         */
        private BigDecimal erc20Revenue = BigDecimal.ZERO;

        /**
         * Dao下已领取erc20数量
         */
        private BigDecimal erc20Collected = BigDecimal.ZERO;

        /**
         * Dao下未领取erc20数量
         */
        private BigDecimal erc20Collectable = BigDecimal.ZERO;

        /**
         * Dao下已领取eth数量
         */
        private String ethCollected = BigDecimal.ZERO.toPlainString();

        /**
         * Dao下未领取eth数量
         */
        private String ethCollectable = BigDecimal.ZERO.toPlainString();

        /**
         * Dao下未领取erc20数量可兑换eth数量
         */
        private BigDecimal erc20CollectableEth = BigDecimal.ZERO;

        /**
         * 用户当前拥有的erc20数量
         */
        private BigDecimal erc20Balance = BigDecimal.ZERO;

        /**
         * 用户当前拥有的erc20数量可兑换eth的数量
         */
        private BigDecimal erc20BalanceEth = BigDecimal.ZERO;

        /**
         * 所有未兑换的eth总数
         */
        private BigDecimal daoAssetPool = BigDecimal.ZERO;

        /**
         * 未兑换的代币数量
         */
        private BigDecimal unchangedTokenAmount = BigDecimal.ZERO;

        /**
         * 兑换eth比例
         */
        private BigDecimal swapEthRatio = BigDecimal.ZERO;

        /**
         * 铸造收益列表
         */
        private List<MintReward> mintRewardList = new ArrayList<>();

        /**
         * 已领取记录列表
         */
        private List<Erc20Collected> erc20CollectedList = new ArrayList<>();

        /**
         * 未领取记录列表
         */
        private List<Erc20Collectable> erc20CollectableList = new ArrayList<>();


        /**
         * 1.7 所属的支付货币类型
         */
        private String payCurrencyType;


        /**
         * 1.7 input token的address
         */
        private String inputTokenAddress;

        /**
         * 1.7 work所属的input token的decimals
         */
        private Integer inputTokenDecimals;

        /**
         * project对应的erc20 token地址
         */
        private String daoErc20Address;

    }

    /**
     * 铸造收益列表
     */
    @Data
    static class MintReward {
        /**
         * canvas名称
         */
        private String canvasName;

        /**
         * canvas铸造收益
         */
        private BigDecimal ethAmount;

    }

    /**
     * 已领取erc20列表
     */
    @Data
    static class Erc20Collected {

        /**
         * canvas名称或者Dao creator reward
         */
        private String collectedName;

        /**
         * dao 编号
         */
        private String daoNumber;

        /**
         * canvas 编号
         */
        private String canvasNumber;

        /**
         * 领取token数量
         */
        private BigDecimal collectedAmount;

    }

    /**
     * 未领取erc20列表
     */
    @Data
    static class Erc20Collectable {

        /**
         * canvas名称或者Dao creator reward
         */
        private String collectedName;

        /**
         * dao 编号
         */
        private String daoNumber;

        /**
         * canvas 编号
         */
        private String canvasNumber;

        /**
         * 待领取token数量
         */
        private BigDecimal collectedAmount;

        /**
         * 是否停机 0-未停机 1-已停机
         */
        private Integer isPaused;

        /**
         * 停机提示信息
         */
        private String pauseedMsg;

    }

}
