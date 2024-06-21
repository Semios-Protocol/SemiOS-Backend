package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.UserHarvestToken;
import semios.api.model.enums.CanvasStatusEnum;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 用户钱包展示已领取token
 *
 * @description: wallet income
 * @author: xiangbin
 * @create: 2022-08-08 11:20
 **/
@Data
public class UserWalletUnclaimedIncomeVo {

    /**
     * canvasId
     */
    private List<String> canvasIdList = new ArrayList<>();

    /**
     * daoId
     */
    private List<String> projectIdList = new ArrayList<>();

    /**
     * canvasId
     */
    private List<String> canvasId2List = new ArrayList<>();

    /**
     * daoId
     */
    private List<String> projectId2List = new ArrayList<>();

    /**
     * 未领取列表
     */
    private List<UnclaimedIncomeVo> unclaimedIncomeVos = new ArrayList<>();

    /**
     * 以ERC20为单位展示所有已领取的信息 返回所有erc20大于0的数据
     *
     * @param daoList
     * @param canvasList
     * @param userHarvestTokenList
     * @return
     */
    public static UserWalletUnclaimedIncomeVo transfer(List<Dao> daoList, List<Canvas> canvasList,
                                                       List<UserHarvestToken> userHarvestTokenList) {
        UserWalletUnclaimedIncomeVo userWalletUnclaimedIncomeVo = new UserWalletUnclaimedIncomeVo();
        List<UnclaimedIncomeVo> unclaimedIncomeVoList = new ArrayList<>();
        List<String> projectIdList = new ArrayList<>();
        List<String> canvasIdList = new ArrayList<>();
        List<String> projectId2List = new ArrayList<>();
        List<String> canvasId2List = new ArrayList<>();

        Map<Integer, Dao> daoMap = new HashMap<>();
        Map<Integer, Canvas> canvasMap = new HashMap<>();

        Map<Integer, UnclaimedIncomeVo> incomeMap = new HashMap<>();
        for (Dao dao : daoList) {
            daoMap.put(dao.getId(), dao);
            if (!ProtoDaoConstant.D4APause && !dao.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                    && dao.getUnclaimedToken().compareTo(BigDecimal.ZERO) > 0) {
                UnclaimedIncomeVo unclaimedIncomeVo = userWalletUnclaimedIncomeVo.new UnclaimedIncomeVo();
                unclaimedIncomeVo.setErc20Number(dao.getDaoNumber() + "");
                unclaimedIncomeVo.setDaoSymbol(dao.getDaoSymbol());
                unclaimedIncomeVo.setErc20Balance(new BigDecimal(String.valueOf(dao.getUnclaimedToken())));
                unclaimedIncomeVo.setEthBalance(new BigDecimal(String.valueOf(dao.getUnclaimedEth())));
                unclaimedIncomeVo.setProjectId(dao.getProjectId());
                incomeMap.put(dao.getDaoNumber(), unclaimedIncomeVo);
                if (dao.getDaoVersion() == 3) {
                    projectId2List.add(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                } else {
                    projectIdList.add(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
                }

            }
        }
        for (Canvas canvas : canvasList) {
            canvasMap.put(canvas.getId(), canvas);
            if (ProtoDaoConstant.D4APause || canvas.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                    || canvas.getCanvasStatus().equals(CanvasStatusEnum.SHUT_DOWN.getStatus())
                    || canvas.getUnclaimedToken().compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            Dao canvasDao = null;
            if (daoMap.get(canvas.getDaoId()) == null) {
                IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
                if (daoService != null) {
                    canvasDao = daoService.getById(canvas.getDaoId());
                }
            } else {
                canvasDao = daoMap.get(canvas.getDaoId());
            }
            if (canvasDao != null && canvasDao.getDaoVersion() == 3) {
                canvasId2List.add(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
            } else {
                canvasIdList.add(CommonUtil.addHexPrefixIfNotExist(canvas.getCanvasId()));
            }

            if (incomeMap.get(canvas.getDaoNumber()) == null) {
                UnclaimedIncomeVo unclaimedIncomeVo = userWalletUnclaimedIncomeVo.new UnclaimedIncomeVo();
                unclaimedIncomeVo.setErc20Number(canvas.getDaoNumber() + "");
                unclaimedIncomeVo.setDaoSymbol(canvas.getDaoSymbol());
                unclaimedIncomeVo.setErc20Balance(new BigDecimal(String.valueOf(canvas.getUnclaimedToken())));
                unclaimedIncomeVo.setEthBalance(new BigDecimal(String.valueOf(canvas.getUnclaimedEth())));
                unclaimedIncomeVo.setProjectId(canvas.getProjectId());
                unclaimedIncomeVo.setCanvasId(canvas.getCanvasId());
                incomeMap.put(canvas.getDaoNumber(), unclaimedIncomeVo);
            } else {
                UnclaimedIncomeVo unclaimedIncomeVo = incomeMap.get(canvas.getDaoNumber());
                BigDecimal balance = unclaimedIncomeVo.getErc20Balance();
                BigDecimal ethBalance = unclaimedIncomeVo.getEthBalance();
                unclaimedIncomeVo
                        .setErc20Balance(balance.add(new BigDecimal(String.valueOf(canvas.getUnclaimedToken()))));
                unclaimedIncomeVo
                        .setEthBalance(ethBalance.add(new BigDecimal(String.valueOf(canvas.getUnclaimedEth()))));
                unclaimedIncomeVo.setCanvasId(canvas.getCanvasId());
                incomeMap.put(canvas.getDaoNumber(), unclaimedIncomeVo);
            }
        }
        Dao daoEth = null;
        Canvas canvasEth = null;
        for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
            if (daoMap.get(userHarvestToken.getDaoId()) == null) {
                IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
                if (daoService != null) {
                    daoEth = daoService.getById(userHarvestToken.getDaoId());
                }
            } else {
                daoEth = daoMap.get(userHarvestToken.getDaoId());
            }
            if (canvasMap.get(userHarvestToken.getCanvasId()) == null) {
                ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
                if (canvasService != null) {
                    canvasEth = canvasService.getById(userHarvestToken.getCanvasId());
                }
            } else {
                canvasEth = canvasMap.get(userHarvestToken.getCanvasId());
            }
            if (daoEth != null && canvasEth != null) {
                daoMap.put(daoEth.getId(), daoEth);
                canvasMap.put(canvasEth.getId(), canvasEth);
                if (ProtoDaoConstant.D4APause || canvasEth.getDaoStatus().equals(DaoStatusEnum.SHUT_DOWN.getStatus())
                        || userHarvestToken.getUnclaimedToken().compareTo(BigDecimal.ZERO) <= 0) {
                    continue;
                }
                String projectId = CommonUtil.addHexPrefixIfNotExist(canvasEth.getProjectId());
                String canvasId = CommonUtil.addHexPrefixIfNotExist(canvasEth.getCanvasId());
                if (incomeMap.get(canvasEth.getDaoNumber()) == null) {
                    UnclaimedIncomeVo unclaimedIncomeVo = userWalletUnclaimedIncomeVo.new UnclaimedIncomeVo();
                    unclaimedIncomeVo.setErc20Number(canvasEth.getDaoNumber() + "");
                    unclaimedIncomeVo.setDaoSymbol(canvasEth.getDaoSymbol());
                    unclaimedIncomeVo
                            .setErc20Balance(new BigDecimal(String.valueOf(userHarvestToken.getUnclaimedToken())));
                    unclaimedIncomeVo
                            .setEthBalance(new BigDecimal(String.valueOf(userHarvestToken.getUnclaimedEth())));
                    unclaimedIncomeVo.setProjectId(projectId);
                    unclaimedIncomeVo.setCanvasId(canvasId);
                    incomeMap.put(canvasEth.getDaoNumber(), unclaimedIncomeVo);
                } else {
                    UnclaimedIncomeVo unclaimedIncomeVo = incomeMap.get(canvasEth.getDaoNumber());
                    BigDecimal balance = unclaimedIncomeVo.getErc20Balance();
                    BigDecimal ethBalance = unclaimedIncomeVo.getEthBalance();
                    unclaimedIncomeVo.setErc20Balance(
                            balance.add(new BigDecimal(String.valueOf(userHarvestToken.getUnclaimedToken()))));
                    unclaimedIncomeVo.setEthBalance(
                            ethBalance.add(new BigDecimal(String.valueOf(userHarvestToken.getUnclaimedEth()))));
                    unclaimedIncomeVo.setCanvasId(canvasId);
                    incomeMap.put(canvasEth.getDaoNumber(), unclaimedIncomeVo);
                }
                if (!projectIdList.contains(projectId)) {
                    if (daoEth.getDaoVersion() == 3) {
                        projectId2List.add(projectId);
                    } else {
                        projectIdList.add(projectId);
                    }
                }
            }
        }

        for (Integer number : incomeMap.keySet()) {
            unclaimedIncomeVoList.add(incomeMap.get(number));
        }
        userWalletUnclaimedIncomeVo.setCanvasIdList(canvasIdList);
        userWalletUnclaimedIncomeVo.setProjectIdList(projectIdList);
        userWalletUnclaimedIncomeVo.setCanvasId2List(canvasId2List);
        userWalletUnclaimedIncomeVo.setProjectId2List(projectId2List);
        unclaimedIncomeVoList = unclaimedIncomeVoList.stream()
                .sorted(Comparator.comparing(UnclaimedIncomeVo::getErc20Balance).reversed()).collect(Collectors.toList());
        userWalletUnclaimedIncomeVo.setUnclaimedIncomeVos(unclaimedIncomeVoList);
        return userWalletUnclaimedIncomeVo;
    }

    /**
     * 未领取记录
     */
    @Data
    class UnclaimedIncomeVo {
        /**
         * erc20编号 D4A_T
         */
        private String erc20Number;

        /**
         * dao symbol
         */
        private String daoSymbol;

        /**
         * 用户当前拥有的erc20数量
         */
        private BigDecimal erc20Balance;

        /**
         * 用户当前拥有的eth数量
         */
        private BigDecimal ethBalance;

        /**
         * DAO 的projectId
         */
        private String projectId;

        /**
         * Canvas的canvasId
         */
        private String canvasId;
    }

}
