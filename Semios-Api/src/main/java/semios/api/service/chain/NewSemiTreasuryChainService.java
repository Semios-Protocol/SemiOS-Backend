package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.enums.DaoTogetherTypeEnum;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import javax.annotation.Resource;
import java.util.List;

/*
 * 创建Main dao用于给国库打款赠送的erc721的地址
 * bytes32 daoId,  //具体dao project id
 * address treasury, //dao国库地址
 * address grantTreasuryNft // 用于给国库打款赠送的erc721的地址
 * uint256 initTokenSupply // 国库初始token数量
 * */
@Slf4j
@Service
public class NewSemiTreasuryChainService implements SubscriberChainService {

    @Resource
    private IDaoService daoService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewSemiTreasuryChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0)); //dao的project id
        String treasury = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))); //dao国库地址
        String grantTreasuryErc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2))); //用于给国库打款赠送的erc721的地址
        String initTokenSupply = CommonUtil.hexToTenString(dataList.get(3)); //国库初始token数量

        // main dao 更新
        Dao dao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());
        if (dao == null) {
            log.error("[NewSemiTreasuryChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiTreasuryChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setTreasuryErc20(treasury);
        updateDao.setGrantTreasuryNftErc721(grantTreasuryErc721Address);

        daoService.updateById(updateDao);


        // 聚合dao 更新
        Dao togetherDao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus());
        if (togetherDao == null) {
            log.error("[NewSemiTreasuryChainService] together dao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiTreasuryChainService cannot find together dao");
        }

        Dao updatetTogetherDao = new Dao();
        updatetTogetherDao.setId(togetherDao.getId());
        updatetTogetherDao.setTreasuryErc20(treasury);

        daoService.updateById(updatetTogetherDao);
    }
}
