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
 * 创建dao给sub dao 打款生成的erc721地址
 * bytes32 daoId,  //具体dao project id
 * address daoNft, //dao原erc721的地址
 * address grantDaoNft // 打款赠送的erc721的地址
 * */
@Slf4j
@Service
public class NewSemiDaoErc721AddressChainService implements SubscriberChainService {

    @Resource
    private IDaoService daoService;

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewSemiDaoErc721AddressChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0)); //dao的project id
        String daoNftErc721 = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(1))); //dao原erc721的地址
        String grantDaoNftErc721Address = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2))); //用于给sub dao打款赠送的erc721的地址

        Dao dao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());
        if (dao == null) {
            log.error("[NewSemiDaoErc721AddressChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("NewSemiDaoErc721AddressChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setDaoNftErc721(daoNftErc721);
        updateDao.setGrantDaoNftErc721(grantDaoNftErc721Address);

        daoService.updateById(updateDao);
    }
}
