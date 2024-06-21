package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAllocationStrategy;
import semios.api.model.enums.DaoRoyaltyTypeEnum;
import semios.api.service.IDaoAllocationStrategyService;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * 1.3 新建dao抛出事件
 * <p>
 * event ChildrenSet(
 * bytes32 daoId,
 * bytes32[] childrenDaoId,
 * uint256[] erc20Ratios,
 * uint256[] ethRatios,
 * uint256 redeemPoolRatioETH,
 * uint256 selfRewardRatioERC20,
 * uint256 selfRewardRatioETH
 * );
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-11-10
 **/
@Slf4j
@Service
public class ChildrenSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;

    public static void main(String[] args) {
        //https://goerli.etherscan.io/tx/0x8ef8593833e0694f81da40af2507f904563855ba7979aeadfa133588a11c3a6f#eventlog
        String data =
                "0x981c3102e3cc7c2ab4463a0833a83ae881cfe723c4b5f1c865476f3bf84c758700000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000001a00000000000000000000000000000000000000000000000000000000000000058000000000000000000000000000000000000000000000000000000000000005a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000025e01994b96e87176c4a85c96bdd0bd45c479cdc4d9bd64dcb25dc31e3f47dd49ebd6dbafb98eef9baf2ca0296c22e7dd7d15af5253a99983862c9f591729bc5d000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000030000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);
        // 与创建的DAO相关联的DAO id
//        String childrenDaoId = CommonUtil.hexToTenString(dataList.get(1));
//        // 关联DAO的ERC20分配比例
//        String erc20Ratios = CommonUtil.hexToTenString(dataList.get(2));
//        // 相关联DAO的ETH分配比例
//        String ethRatios = CommonUtil.hexToTenString(dataList.get(3));
        // Redeem池的ETH分配比例
        String redeemPoolRatioETH = CommonUtil.hexToTenString(dataList.get(4));
        // 留给自己的ERC20分配比例
        String selfRewardRatioERC20 = CommonUtil.hexToTenString(dataList.get(5));
        // 留给自己的ETH分配比例
        String selfRewardRatioETH = CommonUtil.hexToTenString(dataList.get(6));

        //1. 解析参数
        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(1)))) >> 5;
        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(2)))) >> 5;
        int c = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(3)))) >> 5;
        // 与创建的DAO相关联的DAO id
        List<String> childrenDaoIdList = new ArrayList<>();
        // 关联DAO的ERC20分配比例
        List<String> erc20RatiosList = new ArrayList<>();
        // 相关联DAO的ETH分配比例
        List<String> ethRatiosList = new ArrayList<>();
        for (int i = a + 1; i < b; i++) {
            childrenDaoIdList.add(dataList.get(i));
        }
        for (int i = b + 1; i < c; i++) {
            erc20RatiosList.add(CommonUtil.hexToTenString(dataList.get(i)));
        }
        for (int i = c + 1; i < dataList.size(); i++) {
            ethRatiosList.add(CommonUtil.hexToTenString(dataList.get(i)));
        }

        System.out.println("123");
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[ChildrenSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);

        // Redeem池的ETH分配比例
        String redeemPoolRatioEth = CommonUtil.hexToTenString(dataList.get(4)); // 合约修改字段为：redeemPoolInputRatio
        // 留给自己的ERC20分配比例
        String selfRewardRatioErc20 = CommonUtil.hexToTenString(dataList.get(5));   // 合约修改字段为：selfRewardOutputRatio
        // 留给自己的ETH分配比例
        String selfRewardRatioEth = CommonUtil.hexToTenString(dataList.get(6)); // 合约修改字段为：selfRewardInputRatio

        //1. 解析参数
        int a = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(1)))) >> 5;
        int b = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(2)))) >> 5;
        int c = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(3)))) >> 5;
        // 与创建的DAO相关联的DAO id
        List<String> childrenDaoIdList = new ArrayList<>();
        // 关联DAO的ERC20分配比例
        List<String> erc20RatiosList = new ArrayList<>();   // 合约修改字段为：outputRatios
        // 相关联DAO的ETH分配比例
        List<String> ethRatiosList = new ArrayList<>(); // 合约修改字段为：inputRatios
        //这里a+1是因为这个dataList返回的是数组，a下标这里是数组长度
        for (int i = a + 1; i < b; i++) {
            childrenDaoIdList.add(dataList.get(i));
        }
        //这里b+1是因为这个dataList返回的是数组，b下标这里是数组长度
        for (int i = b + 1; i < c; i++) {
            erc20RatiosList.add(CommonUtil.hexToTenString(dataList.get(i)));
        }
        //这里c+1是因为这个dataList返回的是数组，c下标这里是数组长度
        for (int i = c + 1; i < dataList.size(); i++) {
            ethRatiosList.add(CommonUtil.hexToTenString(dataList.get(i)));
        }

        //先删除之前设置的数据
        Integer deleteNum = daoAllocationStrategyService.deleteByOriginProjectIdAndType(projectId, null);
        log.info("[ChildrenSetChainService] projectId:{} deleteNum:{} ", projectId, deleteNum);

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("ChildrenSetChainService cannot find dao");
        }

        //记录当前设置的策略
        List<DaoAllocationStrategy> daoAllocationStrategyList = new ArrayList<>();
        int childrenDaoIdSize = childrenDaoIdList.size();
        if (childrenDaoIdSize > 0) {
            for (int i = 0; i < childrenDaoIdSize; i++) {
                Dao childrenDao = daoService.daoDetailByProjectId(childrenDaoIdList.get(i));
                if (childrenDao == null) {
                    throw new RuntimeException("ChildrenSetChainService cannot find childrenDao id is " + childrenDaoIdList.get(i));
                }
                int erc20Ratios = Integer.parseInt(erc20RatiosList.get(i));
                int ethRatio = Integer.parseInt(ethRatiosList.get(i));
                if (erc20Ratios > 0) {
                    DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
                    daoAllocationStrategy.setOriginProjectId(projectId);
                    daoAllocationStrategy.setType(0);
                    daoAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.ZERO.getType());
                    daoAllocationStrategy.setDaoId(childrenDao.getId());
                    daoAllocationStrategy.setProjectId(childrenDao.getProjectId());
                    daoAllocationStrategy.setDaoName(childrenDao.getDaoName());
                    daoAllocationStrategy.setDaoNumber(childrenDao.getDaoNumber());
                    daoAllocationStrategy.setRoyaltyProportion(new BigDecimal(erc20Ratios).divide(new BigDecimal("100"), 6, BigDecimal.ROUND_FLOOR));
                    daoAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

                    daoAllocationStrategyList.add(daoAllocationStrategy);
                }
                if (ethRatio > 0) {
                    DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
                    daoAllocationStrategy.setOriginProjectId(projectId);
                    daoAllocationStrategy.setType(1);
                    daoAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.ZERO.getType());
                    daoAllocationStrategy.setDaoId(childrenDao.getId());
                    daoAllocationStrategy.setProjectId(childrenDao.getProjectId());
                    daoAllocationStrategy.setDaoName(childrenDao.getDaoName());
                    daoAllocationStrategy.setDaoNumber(childrenDao.getDaoNumber());
                    daoAllocationStrategy.setRoyaltyProportion(new BigDecimal(ethRatio).divide(new BigDecimal("100"), 6, BigDecimal.ROUND_FLOOR));
                    daoAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
                    daoAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

                    daoAllocationStrategyList.add(daoAllocationStrategy);
                }


            }
        }


        if (StringUtils.isNotBlank(redeemPoolRatioEth)) {
            DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
            daoAllocationStrategy.setOriginProjectId(projectId);
            daoAllocationStrategy.setType(1);
            daoAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.ONE.getType());
            daoAllocationStrategy.setDaoId(dao.getId());
            daoAllocationStrategy.setProjectId(dao.getProjectId());
            daoAllocationStrategy.setDaoName(dao.getDaoName());
            daoAllocationStrategy.setDaoNumber(dao.getDaoNumber());
            daoAllocationStrategy.setRoyaltyProportion(new BigDecimal(redeemPoolRatioEth).divide(new BigDecimal("100"), 6, BigDecimal.ROUND_FLOOR));
            daoAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

            daoAllocationStrategyList.add(daoAllocationStrategy);
        }

        if (StringUtils.isNotBlank(selfRewardRatioErc20)) {
            DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
            daoAllocationStrategy.setOriginProjectId(projectId);
            daoAllocationStrategy.setType(0);
            daoAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.TWO.getType());
            daoAllocationStrategy.setDaoId(dao.getId());
            daoAllocationStrategy.setProjectId(dao.getProjectId());
            daoAllocationStrategy.setDaoName(dao.getDaoName());
            daoAllocationStrategy.setDaoNumber(dao.getDaoNumber());
            daoAllocationStrategy.setRoyaltyProportion(new BigDecimal(selfRewardRatioErc20).divide(new BigDecimal("100"), 6, BigDecimal.ROUND_FLOOR));
            daoAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

            daoAllocationStrategyList.add(daoAllocationStrategy);
        }

        if (StringUtils.isNotBlank(selfRewardRatioEth)) {
            DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
            daoAllocationStrategy.setOriginProjectId(projectId);
            daoAllocationStrategy.setType(1);
            daoAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.TWO.getType());
            daoAllocationStrategy.setDaoId(dao.getId());
            daoAllocationStrategy.setProjectId(dao.getProjectId());
            daoAllocationStrategy.setDaoName(dao.getDaoName());
            daoAllocationStrategy.setDaoNumber(dao.getDaoNumber());
            daoAllocationStrategy.setRoyaltyProportion(new BigDecimal(selfRewardRatioEth).divide(new BigDecimal("100"), 6, BigDecimal.ROUND_FLOOR));
            daoAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
            daoAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

            daoAllocationStrategyList.add(daoAllocationStrategy);
        }

        DaoAllocationStrategy daoAllocationStrategy = new DaoAllocationStrategy();
        daoAllocationStrategy.setOriginProjectId(projectId);
        daoAllocationStrategy.setType(0);
        daoAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.THREE.getType());
        daoAllocationStrategy.setDaoId(dao.getId());
        daoAllocationStrategy.setProjectId(dao.getProjectId());
        daoAllocationStrategy.setDaoName(dao.getDaoName());
        daoAllocationStrategy.setDaoNumber(dao.getDaoNumber());

        BigDecimal daoTokenStrategy = daoAllocationStrategyList.stream().filter(v -> v.getType() == 0).map(DaoAllocationStrategy::getRoyaltyProportion).reduce(BigDecimal.ZERO, BigDecimal::add);
        daoAllocationStrategy.setRoyaltyProportion(new BigDecimal("100").subtract(daoTokenStrategy));
        daoAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
        daoAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

        daoAllocationStrategyList.add(daoAllocationStrategy);

        DaoAllocationStrategy daoEthAllocationStrategy = new DaoAllocationStrategy();
        daoEthAllocationStrategy.setOriginProjectId(projectId);
        daoEthAllocationStrategy.setType(1);
        daoEthAllocationStrategy.setRoyaltyType(DaoRoyaltyTypeEnum.THREE.getType());
        daoEthAllocationStrategy.setDaoId(dao.getId());
        daoEthAllocationStrategy.setProjectId(dao.getProjectId());
        daoEthAllocationStrategy.setDaoName(dao.getDaoName());
        daoEthAllocationStrategy.setDaoNumber(dao.getDaoNumber());
        BigDecimal daoEthStrategy = daoAllocationStrategyList.stream().filter(v -> v.getType() == 1).map(DaoAllocationStrategy::getRoyaltyProportion).reduce(BigDecimal.ZERO, BigDecimal::add);
        daoEthAllocationStrategy.setRoyaltyProportion(new BigDecimal("100").subtract(daoEthStrategy));
        daoEthAllocationStrategy.setTransactionHash(transactionDto.getTransactionHash());
        daoEthAllocationStrategy.setBlockTime(transactionDto.getBlockTime());

        daoAllocationStrategyList.add(daoEthAllocationStrategy);

        daoAllocationStrategyService.saveBatch(daoAllocationStrategyList);
    }
}
