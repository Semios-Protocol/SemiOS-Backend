//package semios.api.service.chain;
//
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.stereotype.Service;
//import semios.api.model.dto.common.ProtoDaoConstant;
//import semios.api.model.dto.response.TransactionDto;
//import semios.api.model.entity.Dao;
//import semios.api.model.entity.Work;
//import semios.api.model.enums.BasicDaoEnum;
//import semios.api.model.enums.WorkStatusEnum;
//import semios.api.service.IDaoService;
//import semios.api.service.IWorkService;
//import semios.api.service.SubscriberChainService;
//import semios.api.utils.CommonUtil;
//import semios.api.utils.JacksonUtil;
//
//import java.math.BigDecimal;
//import java.util.List;
//
///**
// * 1.3 新建dao抛出事件
// *
// * @description:
// * @author: xiangbin
// * @create: 2023-11-10
// **/
//@Deprecated
//@Slf4j
//@Service
//public class CreateContinuousProjectParamEmittedForFundingChainService implements SubscriberChainService {
//
//    @Autowired
//    private IDaoService daoService;
//
//    @Autowired
//    private IWorkService workService;
//
//
//    @Override
//    public void handleTrade(TransactionDto transactionDto) throws Exception {
//
//        log.info("[CreateContinuousProjectParamEmittedForFundingChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
//
//        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//        String existDaoId = dataList.get(0);
//        String projectId = dataList.get(1);
//        String dailyMintCap = CommonUtil.hexToTenString(dataList.get(2));
//        String needMintableWork = CommonUtil.hexToTenString(dataList.get(3));
//        String unifiedPriceModeOff = CommonUtil.hexToTenString(dataList.get(4));
//        String unifiedPrice = CommonUtil.hexToTenString(dataList.get(5));
//        //设置自动生成work的总数量 0-为未设置
//        String reserveNftNumber = CommonUtil.hexToTenString(dataList.get(6));
//        String topUpMode = CommonUtil.hexToTenString(dataList.get(7));
//
//
//        Dao dao = daoService.daoDetailByProjectId(projectId);
//        if (dao == null) {
//            throw new RuntimeException("CreateContinuousProjectParamEmittedForFundingChainService cannot find dao");
//        }
//        Dao updateDao = new Dao();
//        updateDao.setId(dao.getId());
//        if (!"0000000000000000000000000000000000000000000000000000000000000000".equals(existDaoId) && !projectId.equalsIgnoreCase(existDaoId)) {
//            updateDao.setExistDaoId(existDaoId);
//            updateDao.setBasicDao(BasicDaoEnum.PROTO_DAO.getBasicType());
//        }
//
//
//        if (StringUtils.isNotBlank(dailyMintCap)) {
//            updateDao.setDailyMintCap("0".equals(dailyMintCap) ? 10000 : Integer.parseInt(dailyMintCap));
//        }
//        updateDao.setAddWork("0".equals(needMintableWork) ? 1 : 0);
//        updateDao.setNeedMintableWork("0".equals(needMintableWork) ? 1 : 0);
//        updateDao.setTopupMode("0".equals(topUpMode) ? 0 : 1);
//        if ("0".equals(unifiedPriceModeOff)) {
//            updateDao.setGlobalDaoPrice(new BigDecimal(unifiedPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//            updateDao.setDaoFloorPrice(new BigDecimal(unifiedPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//            updateDao.setCanvasFloorPrice(new BigDecimal(unifiedPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//        } else {
//            //-1为关闭
//            updateDao.setGlobalDaoPrice(BigDecimal.ONE.negate());
//        }
//        if (StringUtils.isNotBlank(reserveNftNumber)) {
//            updateDao.setGenerateWorkSet(Integer.valueOf(reserveNftNumber));
//        }
//
//
//        daoService.updateById(updateDao);
//        //如果已经生成了则删掉
//        if (updateDao.getNeedMintableWork() == 1) {
//            Work work = workService.selectLastGenerateWork(dao.getId());
//            if (work != null) {
//                log.info("[CreateContinuousProjectParamEmittedForFundingChainService] updateDao:{} workId:{}", dao.getId(), work.getId());
//                work.setWorkStatus(WorkStatusEnum.EXPIRED.getStatus());
//                workService.updateById(work);
//            }
//        }
//        log.info("[CreateContinuousProjectParamEmittedForFundingChainService] updateDao:{}", dao.getId() + "basic dao to proto dao");
//
//    }
//
//
//    public static void main(String[] args) {
//        //https://goerli.etherscan.io/tx/0x8ef8593833e0694f81da40af2507f904563855ba7979aeadfa133588a11c3a6f#eventlog
//        String data =
//                "0x000000000000000000000000000000000000000000000000000000000000000086c28aafc0def9ece950e21fac59f4ee28a3b5d36b2cd07a1fcc3e86d36e3adb000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002386f26fc1000000000000000000000000000000000000000000000000000000000000000003e80000000000000000000000000000000000000000000000000000000000000000";
//        List<String> dataList = CommonUtil.splitBy32Bytes(data);
//
//
//        String existDaoId = dataList.get(0);
//        String projectId = dataList.get(1);
//        String dailyMintCap = CommonUtil.hexToTenString(dataList.get(2));
//        String needMintableWork = CommonUtil.hexToTenString(dataList.get(3));
//        String unifiedPriceModeOff = CommonUtil.hexToTenString(dataList.get(4));
//        String unifiedPrice = CommonUtil.hexToTenString(dataList.get(5));
//        //设置自动生成work的总数量 0-为未设置
//        String reserveNftNumber = CommonUtil.hexToTenString(dataList.get(6));
//        String topUpMode = CommonUtil.hexToTenString(dataList.get(7));
//
//
//        Dao dao = new Dao();
//        if (dao == null) {
//            throw new RuntimeException("CreateContinuousProjectParamEmittedForFundingChainService cannot find dao");
//        }
//        if (!"0000000000000000000000000000000000000000000000000000000000000000".equals(existDaoId) && !projectId.equalsIgnoreCase(existDaoId)) {
//            dao.setExistDaoId(existDaoId);
//            dao.setBasicDao(BasicDaoEnum.PROTO_DAO.getBasicType());
//        }
//
//
//        if (StringUtils.isNotBlank(dailyMintCap)) {
//            dao.setDailyMintCap("0".equals(dailyMintCap) ? 10000 : Integer.parseInt(dailyMintCap));
//        }
//        dao.setAddWork("0".equals(needMintableWork) ? 1 : 0);
//        dao.setNeedMintableWork("0".equals(needMintableWork) ? 1 : 0);
//        dao.setTopupMode("0".equals(topUpMode) ? 0 : 1);
//        if ("0".equals(unifiedPriceModeOff)) {
//            dao.setGlobalDaoPrice(new BigDecimal(unifiedPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//            dao.setDaoFloorPrice(new BigDecimal(unifiedPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//            dao.setCanvasFloorPrice(new BigDecimal(unifiedPrice).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
//        } else {
//            //-1为关闭
//            dao.setGlobalDaoPrice(BigDecimal.ONE.negate());
//        }
//        if (StringUtils.isNotBlank(reserveNftNumber)) {
//            dao.setGenerateWorkSet(Integer.valueOf(reserveNftNumber));
//        }
//    }
//}
