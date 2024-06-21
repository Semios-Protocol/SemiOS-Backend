package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * dao unifiedPrice修改事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class DaoUnifiedPriceSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    public static void main(String[] args) {
        //https://goerli.etherscan.io/tx/0x70ba9daf476908aaabfe27a5166925154d68b535d9bb3953780dd8e94c04a03f#eventlog
        String data = "0x36a78967d1afef3ee3eb4b73fe2be610cc2e16b9af4f9d8f9440938d7627d8d600000000000000000000000000000000000000000000000000470de4df820000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0));
        String newUnifiedPrice = CommonUtil.hexToTenString(dataList.get(1));
        System.out.println("project_id" + " is " + projectId);
        System.out.println("newUnifiedPrice" + " is " + newUnifiedPrice);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoUnifiedPriceSetChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));


        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0));
        String newUnifiedPrice = CommonUtil.hexToTenString(dataList.get(1));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("DaoUnifiedPriceSetChainService cannot find dao");
        }

        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            Dao updateDao = new Dao();
            updateDao.setId(dao.getId());
            log.info("[DaoUnifiedPriceSetChainService] updateDao:{} oldUnifiedPrice:{} newUnifiedPrice is:{}", dao.getId(), dao.getGlobalDaoPrice(), newUnifiedPrice);


            if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {
                updateDao.setGlobalDaoPrice(new BigDecimal(newUnifiedPrice).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())));
                updateDao.setDaoFloorPrice(new BigDecimal(newUnifiedPrice).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())));
            } else {
                updateDao.setGlobalDaoPrice(new BigDecimal(newUnifiedPrice).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
                updateDao.setDaoFloorPrice(new BigDecimal(newUnifiedPrice).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
            }

            updateDao.setCanvasFloorPrice(updateDao.getDaoFloorPrice());

            daoService.updateById(updateDao);
            log.info("[DaoUnifiedPriceSetChainService] updateDao:{} newUnifiedPrice to:{}", dao.getId(), updateDao.getErc20TotalSupply());
        } else {
            log.info("[DaoUnifiedPriceSetChainService] updateDao:{} not set unifiedPrice", dao.getId());
        }


    }
}
