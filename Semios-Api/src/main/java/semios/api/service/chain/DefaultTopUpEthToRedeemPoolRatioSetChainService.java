//package semios.api.service.chain;
//
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Service;
//import semios.api.model.dto.response.TransactionDto;
//import semios.api.model.entity.Dao;
//import semios.api.model.enums.DaoTogetherTypeEnum;
//import semios.api.service.IDaoService;
//import semios.api.service.SubscriberChainService;
//import semios.api.utils.CommonUtil;
//import semios.api.utils.JacksonUtil;
//
//import javax.annotation.Resource;
//import java.math.BigDecimal;
//import java.util.List;
//
///*
// * 修改together dao默认值
// * 修改默认eth-token的比例
// * bytes32 daoId,  //聚合dao project id
// * uint256 ethToRedeemPoolRatio // 比例*100
// * */
//@Deprecated
//@Slf4j
//@Service
//public class DefaultTopUpEthToRedeemPoolRatioSetChainService implements SubscriberChainService {
//    @Resource
//    private IDaoService daoService;
//
//    @Override
//    public void handleTrade(TransactionDto transactionDto) throws Exception {
//        // 已经不抛
//        log.info("[DefaultTopUpEthToRedeemPoolRatioSetChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));
//
//        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0)); //dao的project id
//        String ethTransTokenRatio = CommonUtil.hexToTenString(dataList.get(1)); //eth to token 的比例
//
//        // 赋值给聚合dao
//        Dao dao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus());
//        if (dao == null) {
//            log.error("[DefaultTopUpEthToRedeemPoolRatioSetChainService] dao not find projectId:{}", projectId);
//            throw new RuntimeException("DefaultTopUpEthToRedeemPoolRatioSetChainService cannot find dao");
//        }
//
//        Dao updateDao = new Dao();
//        updateDao.setId(dao.getId());
//
//        BigDecimal ratio = new BigDecimal(ethTransTokenRatio).divide(new BigDecimal("100"));
//        updateDao.setEthTokenRoyalty(ratio);
//
//        daoService.updateById(updateDao);
//    }
//}
