package semios.api.model.vo.res.DaoExportInfo;

import lombok.Data;
import semios.api.model.dto.chain.DaoEthRoyaltyToken;
import semios.api.model.dto.chain.DaoReserveRatio;
import semios.api.model.dto.chain.DaoRoyaltyToken;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAllocationStrategy;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.vo.req.DaoExportInfoParam.DaoExportParam;
import semios.api.model.vo.res.DaoAllocationVo;
import semios.api.service.IDaoAllocationStrategyService;
import semios.api.utils.JacksonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;


@Data
public class NodesTokenStructure {

    /**
     * 乐透模式 线性DRB释放ERC20 0-关闭 1-开启 对应isProgressiveJackpot
     */
    private Integer royaltyTokenLotteryMode;

    /**
     * eth分配比例 ETH Allocation
     * For Seed Nodes Redeem Pool
     * This Nodes Internal Incentives
     * This Nodes Reserves
     */
    // This Nodes Reserves = (100 - For Seed Nodes Redeem Pool - This Nodes Internal Incentives)
    private ETHAllocation ethAllocation;

    /**
     * daoToken分配比例 ERC-20 Allocation
     * This Nodes Internal Incentives
     * This Nodes Reserves
     */
    // This Nodes Reserves = (100 - This Nodes Internal Incentives)
    private Erc20Allocation daoAllocation;

    // Assets Interacts with Other Nodes

    /**
     * DAO token allocation
     */
    private List<DaoAllocationVo> daoTokenAllocationVos = new ArrayList<>();
    /**
     * DAO eth allocation
     */
    private List<DaoAllocationVo> daoEthAllocationVos = new ArrayList<>();


    /**
     * ERC-721 Mint Fee  Floating Mint price
     * dao 表中的 unfixedReserveRatio 字段
     * d4aMintFee--Semios Fee
     * daoMintFee--Subnodes Fee
     * redeemPoolMintFee--Seed Nodes Fee
     * canvasMintFee--Builder Fee
     */
    private DaoReserveRatio floatingMintPrice;

    /**
     * ERC-721 Mint Fee  Fixed Mint price
     * dao 表中的 fixedReserveRatio 字段
     * d4aMintFee--Semios Fee
     * daoMintFee--Subnodes Fee
     * redeemPoolMintFee--Seed Nodes Fee
     * canvasMintFee--Builder Fee
     */
    private DaoReserveRatio fixedMintPrice;

    /**
     * ERC-20 Incentives
     * dao 表中的 royaltyToken 字段
     * daoReward--Starter Incentives
     * d4aReward--Semios Incentives
     * canvasReward--Builder Incentives
     * minterReward--Minter Incentives
     */
    private DaoRoyaltyToken erc20Incentives;


    /**
     * ETH Incentives
     * dao 表中的 ethRoyaltyToken 字段
     * daoCreatorETHReward--Starter Incentives
     * d4aReward--Semios Incentives
     * canvasCreatorETHReward--Builder Incentives
     * minterETHReward--Minter Incentives
     */
    private DaoEthRoyaltyToken ethIncentives;


    public static NodesTokenStructure tranferNodesTokenStructure(Dao dao, DaoExportParam daoExportParam) {
        NodesTokenStructure daoBenefitsDistributeResVo = new NodesTokenStructure();

        IDaoAllocationStrategyService daoAllocationStrategyService = SpringBeanUtil.getBean(IDaoAllocationStrategyService.class);
        if (daoAllocationStrategyService == null) {
            return daoBenefitsDistributeResVo;
        }

        daoBenefitsDistributeResVo.setRoyaltyTokenLotteryMode(dao.getRoyaltyTokenLotteryMode());


        List<DaoAllocationStrategy> daoAllocationStrategyList = daoAllocationStrategyService.selectByOriginProjectIdAndType(dao.getProjectId(), null);

        List<BigDecimal> ethAllocationList = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() != 0).sorted(Comparator.comparing(DaoAllocationStrategy::getRoyaltyType)).map(DaoAllocationStrategy::getRoyaltyProportion).collect(Collectors.toList());
        ETHAllocation ethAllocation = new ETHAllocation();
        ethAllocation.setSeedNodesRedeemPool(ethAllocationList.get(0).toPlainString());
        ethAllocation.setNodesInternalIncentives(ethAllocationList.get(1).toPlainString());
        // 如果导出的是sub node，需要计算到给其他node的分配比例
        if (daoExportParam.getType().equals(1)){
            List<DaoAllocationVo> daoEthAllocationVos = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
            daoBenefitsDistributeResVo.setDaoEthAllocationVos(daoEthAllocationVos);
            if (!daoEthAllocationVos.isEmpty()){
                // 计算出其他node所有的royaltyProportion之和
                BigDecimal sumRoyaltyProportionEth = daoEthAllocationVos.stream().map(DaoAllocationVo::getRoyaltyProportion).reduce(BigDecimal.ZERO, BigDecimal::add);
                ethAllocation.setEthToOtherNodes(sumRoyaltyProportionEth.toPlainString());
            }
        }
        ethAllocation.setNodesReserves(new BigDecimal(100)
                .subtract(new BigDecimal(ethAllocation.getSeedNodesRedeemPool()))
                .subtract(new BigDecimal(ethAllocation.getNodesInternalIncentives()))
                .subtract(new BigDecimal(ethAllocation.getEthToOtherNodes()))   // 如果是seed node，默认为0，不影响计算
                .toPlainString());
        daoBenefitsDistributeResVo.setEthAllocation(ethAllocation);


        List<BigDecimal> daoAllocationList = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() != 0).sorted(Comparator.comparing(DaoAllocationStrategy::getRoyaltyType)).map(DaoAllocationStrategy::getRoyaltyProportion).collect(Collectors.toList());
        Erc20Allocation erc20Allocation = new Erc20Allocation();
        erc20Allocation.setNodesInternalIncentives(daoAllocationList.get(0).toPlainString());
        // 如果导出的是sub node，需要计算到给其他node的分配比例
        if (daoExportParam.getType().equals(1)){
            List<DaoAllocationVo> daoTokenAllocationVos  = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0 && v.getRoyaltyType() == 0).map(DaoAllocationVo::transfer).collect(Collectors.toList());
            daoBenefitsDistributeResVo.setDaoTokenAllocationVos(daoTokenAllocationVos);
            if (!daoTokenAllocationVos.isEmpty()){
                // 计算出所有的royaltyProportion之和
                BigDecimal sumRoyaltyProportionToken = daoTokenAllocationVos.stream().map(DaoAllocationVo::getRoyaltyProportion).reduce(BigDecimal.ZERO, BigDecimal::add);
                erc20Allocation.setErc20TOtherNodes(sumRoyaltyProportionToken.toPlainString());
            }
        }

        // 根据情况判断
        erc20Allocation.setNodesReserves(new BigDecimal(100)
                .subtract(new BigDecimal(erc20Allocation.getNodesInternalIncentives()))
                .subtract(new BigDecimal(erc20Allocation.getErc20TOtherNodes()))   // 如果是seed node，默认为0，不影响计算
                .toPlainString());
        daoBenefitsDistributeResVo.setDaoAllocation(erc20Allocation);

        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode())){
            daoBenefitsDistributeResVo.setFloatingMintPrice(getDefaultDaoReserveRatio());
            daoBenefitsDistributeResVo.setFixedMintPrice(getDefaultDaoReserveRatio());
            daoBenefitsDistributeResVo.setErc20Incentives(getDefaultDaoRoyaltyToken());
            daoBenefitsDistributeResVo.setEthIncentives(getDefaultDaoEthRoyaltyToken());
        }else {
            daoBenefitsDistributeResVo.setFloatingMintPrice(JacksonUtil.json2pojo(dao.getUnfixedReserveRatio(), DaoReserveRatio.class));
            daoBenefitsDistributeResVo.setFixedMintPrice(JacksonUtil.json2pojo(dao.getFixedReserveRatio(), DaoReserveRatio.class));
            daoBenefitsDistributeResVo.setErc20Incentives(JacksonUtil.json2pojo(dao.getRoyaltyToken(), DaoRoyaltyToken.class));
            daoBenefitsDistributeResVo.setEthIncentives(JacksonUtil.json2pojo(dao.getEthRoyaltyToken(), DaoEthRoyaltyToken.class));
        }

        return daoBenefitsDistributeResVo;
    }


    private static DaoReserveRatio getDefaultDaoReserveRatio() {
        DaoReserveRatio daoReserveRatio = new DaoReserveRatio();
        daoReserveRatio.setCanvasMintFee(new BigDecimal("0"));
        daoReserveRatio.setD4aMintFee(new BigDecimal("0"));
        daoReserveRatio.setDaoMintFee(new BigDecimal("2.5"));
        daoReserveRatio.setRedeemPoolMintFee(new BigDecimal("97.5"));
        return daoReserveRatio;
    }


    private static DaoRoyaltyToken getDefaultDaoRoyaltyToken() {
        DaoRoyaltyToken daoRoyaltyToken = new DaoRoyaltyToken();
        daoRoyaltyToken.setCanvasReward(new BigDecimal("25"));
        daoRoyaltyToken.setMinterReward(new BigDecimal("25"));
        daoRoyaltyToken.setDaoReward(new BigDecimal("50"));
        daoRoyaltyToken.setD4aReward(new BigDecimal("0"));
        return daoRoyaltyToken;
    }


    private static DaoEthRoyaltyToken getDefaultDaoEthRoyaltyToken() {
        DaoEthRoyaltyToken daoEthRoyaltyToken = new DaoEthRoyaltyToken();
        daoEthRoyaltyToken.setCanvasCreatorETHReward(new BigDecimal("25"));
        daoEthRoyaltyToken.setMinterETHReward(new BigDecimal("25"));
        daoEthRoyaltyToken.setDaoCreatorETHReward(new BigDecimal("50"));
        daoEthRoyaltyToken.setD4aReward(new BigDecimal("0"));
        return daoEthRoyaltyToken;
    }
}
