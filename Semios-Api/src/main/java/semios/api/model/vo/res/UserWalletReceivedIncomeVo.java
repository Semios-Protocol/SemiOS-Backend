package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.entity.UserHarvestToken;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.TokenReceiveTypeEnum;
import semios.api.model.enums.TokenTypeEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.IDaoService;
import semios.api.service.ITokenReceivedRecordService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 用户钱包展示已领取token
 *
 * @description: wallet income
 * @author: xiangbin
 * @create: 2022-08-08 11:20
 **/
@Slf4j
@Data
public class UserWalletReceivedIncomeVo {

    /**
     * erc20Address
     */
    private String erc20Address;
    /**
     * erc20编号 D4A_T
     */
    private String erc20Number;

    /**
     * 用户当前拥有的erc20数量
     */
    private BigDecimal erc20Balance = BigDecimal.ZERO;

    /**
     * 用户当前拥有的erc20数量可兑换eth数量
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
    private String swapEthRatio = "0";

    /**
     * dao 的projectId
     */
    private String projectId;

    /**
     * 是否停机 0-未停机 1-已停机
     */
    private Integer isPaused = 0;

    /**
     * 停机提示信息
     */
    private String pauseedMsg;

    /**
     * 以ERC20为单位展示所有已领取的信息 返回所有erc20大于0的数据
     *
     * @param daoList
     * @param canvasList
     * @param tokenReceivedRecordList
     * @param userHarvestTokenList
     * @param userAddress
     * @return
     */
    public static List<UserWalletReceivedIncomeVo> transfer(List<Dao> daoList, List<Canvas> canvasList,
                                                            List<TokenReceivedRecord> tokenReceivedRecordList, List<UserHarvestToken> userHarvestTokenList,
                                                            String userAddress) {

//        log.info("[UserWalletReceivedIncomeVo] tokenReceivedRecordList:{}", JacksonUtil.obj2json(tokenReceivedRecordList));
//        log.info("[UserWalletReceivedIncomeVo] daoList:{}", JacksonUtil.obj2json(daoList));
//        log.info("[UserWalletReceivedIncomeVo] canvasList:{}", JacksonUtil.obj2json(canvasList));
//        log.info("[UserWalletReceivedIncomeVo] userHarvestTokenList:{}", JacksonUtil.obj2json(userHarvestTokenList));
        List<UserWalletReceivedIncomeVo> userWalletIncomeVos = new ArrayList<>();
        Map<Integer, BigDecimal> incomeMap = new HashMap<>();
        Dao daoEth = null;
        for (Dao dao : daoList) {

            daoEth = dao;
            //钱包这里topup模式的不展示
            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
                continue;
            }
            incomeMap.put(dao.getDaoNumber(), dao.getReceivedToken());
        }
        for (Canvas canvas : canvasList) {
            if (incomeMap.get(canvas.getDaoNumber()) == null) {
                incomeMap.put(canvas.getDaoNumber(), canvas.getReceivedToken());
            } else {
                BigDecimal token = incomeMap.get(canvas.getDaoNumber());
                incomeMap.put(canvas.getDaoNumber(), canvas.getReceivedToken().add(token));
            }
        }
        for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
            IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
            if (daoService != null && (daoEth == null || !(daoEth.getId().equals(userHarvestToken.getDaoId())))) {
                daoEth = daoService.getById(userHarvestToken.getDaoId());
            }
            if (daoEth != null) {
                //钱包这里topup模式的不展示
                if (TrueOrFalseEnum.TRUE.getStatus().equals(daoEth.getTopupMode())) {
                    continue;
                }
                if (incomeMap.get(daoEth.getDaoNumber()) == null) {
                    incomeMap.put(daoEth.getDaoNumber(), userHarvestToken.getReceivedToken());
                } else {
                    BigDecimal token = incomeMap.get(daoEth.getDaoNumber());
                    incomeMap.put(daoEth.getDaoNumber(), userHarvestToken.getReceivedToken().add(token));
                }
            }
        }

        if (tokenReceivedRecordList.size() > 0) {
            //只筛选类型为transfer的，因为其他类型的已经加减过receivedToken了。
            Map<Integer, List<TokenReceivedRecord>> tokenReceivedMap =
                    tokenReceivedRecordList.stream().filter(v -> v.getDaoNumber() != null && TokenReceiveTypeEnum.TRANSFER.getType().equals(v.getReceiveType()))
                            .collect(Collectors.groupingBy(TokenReceivedRecord::getDaoNumber));
            for (Integer daoNumber : tokenReceivedMap.keySet()) {
                List<TokenReceivedRecord> tokenReceivedRecord = tokenReceivedMap.get(daoNumber);
                List<TokenReceivedRecord> tokenReceivedRecordListTo =
                        tokenReceivedRecord.stream().filter(v -> v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType()))
                                .filter(v -> userAddress.equals(v.getToAddress())).collect(Collectors.toList());
                List<TokenReceivedRecord> tokenReceivedRecordListFrom =
                        tokenReceivedRecord.stream().filter(v -> v.getTokenType().equals(TokenTypeEnum.TRANSFER.getType()))
                                .filter(v -> userAddress.equals(v.getFromAddress())).collect(Collectors.toList());
                List<TokenReceivedRecord> tokenReceivedRecordListSwap =
                        tokenReceivedRecord.stream().filter(v -> v.getTokenType().equals(TokenTypeEnum.SWAP.getType()))
                                .filter(v -> v.getReceiveType().equals(TokenReceiveTypeEnum.TRANSFER.getType()))
                                .filter(v -> userAddress.equals(v.getReceiveAddress())).collect(Collectors.toList());
                if (tokenReceivedRecordListTo.size() > 0 || tokenReceivedRecordListSwap.size() > 0
                        || tokenReceivedRecordListFrom.size() > 0) {
                    // transfer收到的
                    BigDecimal receiveTo = tokenReceivedRecordListTo.stream().map(TokenReceivedRecord::getTokenNum)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);
                    // transfer转出去的
                    BigDecimal receiveFrom = tokenReceivedRecordListFrom.stream().map(TokenReceivedRecord::getTokenNum)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);
                    // swap兑换的
                    BigDecimal swap = tokenReceivedRecordListSwap.stream().map(TokenReceivedRecord::getTokenNum)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);
                    BigDecimal receive = receiveTo.subtract(receiveFrom);
                    //有可能是负值，负值也要计算进来
//                    if (receive.compareTo(swap) > 0) {
                    if (incomeMap.get(daoNumber) == null) {
                        incomeMap.put(daoNumber, receive.subtract(swap));
                    } else {
                        BigDecimal token = incomeMap.get(daoNumber);
                        incomeMap.put(daoNumber, receive.subtract(swap).add(token));
                    }
//                    }

                }

            }

        }
        log.info("[UserWalletReceivedIncomeVo] userAddress:{} incomeMap:{}", userAddress, JacksonUtil.obj2json(incomeMap));

        for (Integer number : incomeMap.keySet()) {
            UserWalletReceivedIncomeVo userWalletIncomeVo = new UserWalletReceivedIncomeVo();

            BigDecimal erc20Balance = new BigDecimal(String.valueOf(incomeMap.get(number)));

            if (erc20Balance.compareTo(BigDecimal.ZERO) == 0) {
                continue;
            }
            userWalletIncomeVo.setErc20Balance(erc20Balance);

            IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
            if (daoService != null) {
                Dao dao = daoService.selectDaoByDaoNumber(number);
                if (dao != null) {
                    //钱包这里topup模式的不展示
                    if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())) {
                        continue;
                    }
                    userWalletIncomeVo.setErc20Address(dao.getErc20Token());
                    userWalletIncomeVo.setErc20Number(dao.getDaoSymbol());
                    if (dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())) {
                        userWalletIncomeVo.setIsPaused(1);
                        userWalletIncomeVo.setPauseedMsg(dao.getDaoName() + " (D4A@" + dao.getDaoNumber()
                                + ") This function is suspended for security reasons.");
                    }
                    if (ProtoDaoConstant.D4APause) {
                        userWalletIncomeVo.setIsPaused(1);
                        userWalletIncomeVo.setPauseedMsg("D4A This function is suspended for security reasons.");
                    }
                    userWalletIncomeVo.setErc20BalanceEth(
                            UserWalletDaoIncomeVo.transferEthForContract(dao, erc20Balance, userAddress));
                    if (erc20Balance.compareTo(BigDecimal.ZERO) < 0) {
                        userWalletIncomeVo.setErc20BalanceEth(
                                UserWalletDaoIncomeVo.transferEthForContract(dao, erc20Balance.negate(), userAddress).negate());
                    }
                    userWalletIncomeVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                    if (userWalletIncomeVo.getErc20Balance() != null
                            && userWalletIncomeVo.getErc20Balance().compareTo(BigDecimal.ZERO) > 0
                            && userWalletIncomeVo.getErc20BalanceEth() != null) {
                        userWalletIncomeVo.setSwapEthRatio(BigDecimal.ONE
                                .divide(userWalletIncomeVo.getErc20Balance(), 18, RoundingMode.FLOOR)
                                .multiply(userWalletIncomeVo.getErc20BalanceEth()).stripTrailingZeros().toPlainString());
                    }
                    // 查询当天兑换的记录
                    ITokenReceivedRecordService tokenReceivedRecordService =
                            SpringBeanUtil.getBean(ITokenReceivedRecordService.class);
                    if (tokenReceivedRecordService != null) {
                        List<TokenReceivedRecord> tokenReceivedRecords =
                                tokenReceivedRecordService.recordListByTokenType(dao.getProjectId(),
                                        Integer.valueOf(dao.getCurrentRound()), TokenTypeEnum.SWAP.getType());
                        if (tokenReceivedRecords == null || tokenReceivedRecords.size() == 0) {
                            userWalletIncomeVo.setDaoAssetPool(
                                    dao.getDaoAssetPool() == null ? BigDecimal.ZERO : dao.getDaoAssetPool());
                            userWalletIncomeVo.setUnchangedTokenAmount(dao.getUnchangedTokenAmount() == null
                                    ? BigDecimal.ZERO : dao.getUnchangedTokenAmount());
                        } else {
                            BigDecimal daoAssetPool = tokenReceivedRecords.stream()
                                    .map(TokenReceivedRecord::getEthAmount).reduce(BigDecimal.ZERO, BigDecimal::add);
                            BigDecimal unchangedTokenAmount =
                                    tokenReceivedRecords.stream().map(v -> new BigDecimal(String.valueOf(v.getTokenNum())))
                                            .reduce(BigDecimal.ZERO, BigDecimal::add);
                            if (dao.getDaoAssetPool() != null) {
                                userWalletIncomeVo.setDaoAssetPool(dao.getDaoAssetPool().subtract(daoAssetPool));
                            }
                            if (dao.getUnchangedTokenAmount() != null) {
                                userWalletIncomeVo.setUnchangedTokenAmount(
                                        dao.getUnchangedTokenAmount().subtract(unchangedTokenAmount));
                            }
                        }
                    }
                } else {
                    continue;
                }
            }

            userWalletIncomeVos.add(userWalletIncomeVo);
        }

        log.info("[UserWalletReceivedIncomeVo] userAddress:{} userWalletIncomeVos:{}", userAddress, JacksonUtil.obj2json(userWalletIncomeVos));

        //把erc20地址相同的聚合为一条数据
        Map<String, UserWalletReceivedIncomeVo> erc20IncomeVoMap = new HashMap<>();
        for (UserWalletReceivedIncomeVo userWalletReceivedIncomeVo : userWalletIncomeVos) {
            UserWalletReceivedIncomeVo userWalletReceivedIncomeVoAdd = erc20IncomeVoMap.get(userWalletReceivedIncomeVo.getErc20Address());
            if (userWalletReceivedIncomeVoAdd == null) {
                erc20IncomeVoMap.put(userWalletReceivedIncomeVo.getErc20Address(), userWalletReceivedIncomeVo);
                continue;
            }
            userWalletReceivedIncomeVoAdd.setErc20Balance(userWalletReceivedIncomeVo.getErc20Balance().add(userWalletReceivedIncomeVoAdd.getErc20Balance()));
            userWalletReceivedIncomeVoAdd.setErc20BalanceEth(userWalletReceivedIncomeVo.getErc20BalanceEth().add(userWalletReceivedIncomeVoAdd.getErc20BalanceEth()));
            userWalletReceivedIncomeVoAdd.setDaoAssetPool(userWalletReceivedIncomeVo.getDaoAssetPool().add(userWalletReceivedIncomeVoAdd.getDaoAssetPool()));
            userWalletReceivedIncomeVoAdd.setUnchangedTokenAmount(userWalletReceivedIncomeVo.getUnchangedTokenAmount().add(userWalletReceivedIncomeVoAdd.getUnchangedTokenAmount()));
            userWalletReceivedIncomeVoAdd.setSwapEthRatio(BigDecimal.ONE
                    .divide(userWalletReceivedIncomeVoAdd.getErc20Balance(), 18, RoundingMode.FLOOR)
                    .multiply(userWalletReceivedIncomeVoAdd.getErc20BalanceEth()).stripTrailingZeros().toPlainString());
            if (userWalletReceivedIncomeVo.getIsPaused() == 1) {
                userWalletReceivedIncomeVoAdd.setIsPaused(userWalletReceivedIncomeVo.getIsPaused());
                userWalletReceivedIncomeVoAdd.setPauseedMsg(userWalletReceivedIncomeVo.getPauseedMsg());
            }

            erc20IncomeVoMap.put(userWalletReceivedIncomeVoAdd.getErc20Address(), userWalletReceivedIncomeVoAdd);
        }

        userWalletIncomeVos = erc20IncomeVoMap.values().stream()
                .sorted(Comparator.comparing(UserWalletReceivedIncomeVo::getErc20Balance).reversed())
                .collect(Collectors.toList());

        return userWalletIncomeVos;
    }

    public static void main(String[] args) {
        // Map<Integer, Integer> incomeMap = new HashMap<>();
        // incomeMap.put(1,2);
        // incomeMap.put(3,2);
        // incomeMap.put(5,2);
        // incomeMap.put(4,2);
        // incomeMap.put(2,2);
        // incomeMap.put(1,4);
        // for (Integer integer : incomeMap.keySet()) {
        // System.out.println(integer +":"+ incomeMap.get(integer));
        // }

        // BigDecimal bigDecimal = BigDecimal.ONE;
        // System.out.println(bigDecimal.divide(new BigDecimal("166666666666666666") , 18,
        // RoundingMode.FLOOR).multiply(new BigDecimal("200")));
        // System.out.println(bigDecimal.divide(new BigDecimal("166666666666666666") , 18,
        // RoundingMode.FLOOR).multiply(new BigDecimal("200")).doubleValue());

        List<TokenReceivedRecord> tokenReceivedRecordListTo = new ArrayList<>();
        BigDecimal receiveTo = tokenReceivedRecordListTo.stream().map(TokenReceivedRecord::getTokenNum)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        System.out.println(receiveTo);

    }
}
