package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.enums.WorkStatusEnum;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * 设置 DAO 地板价
 *
 * @description: 设置 DAO 地板价
 * @author: xiangbin
 * @create: 2023-06-12 13:43
 **/
@Slf4j
@Service
public class DaoFloorPriceSetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkService workService;

    @Autowired
    private CommonService commonService;

    // https://goerli.etherscan.io/tx/0x38f62e4c840f1ffb2278922d165c84fbd621e776a467e94b0bebaef71d18525b#eventlog

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoFloorPriceSetChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));
        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = CommonUtil.addHexPrefixIfNotExist(dataList.get(0));

        String newFloorPrice = CommonUtil.hexToTenString(dataList.get(1));
        if (newFloorPrice == null) {
            throw new RuntimeException("DaoFloorPriceSetChainService newFloorPrice is null");
        }

        Dao dao = daoService.daoDetailByProjectId(CommonUtil.removeHexPrefixIfExists(projectId));
        if (dao == null) {
            log.error("[DaoFloorPriceSetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("DaoFloorPriceSetChainService cannot find dao");
        }
        if (dao.getDaoVersion().equals(1)) {
            log.error("[DaoFloorPriceSetChainService] dao not Support modification daoId:{}", dao.getId());
            throw new RuntimeException("DaoFloorPriceSetChainService dao not Support modification");
        }
        //如果为一口价的dao，则修改daoFloorPrice不生效，都展示一口价 1.1.2
        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            log.warn("[DaoFloorPriceSetChainService] daoId:{} globalDaoPrice:{} newFloorPrice:{} ", dao.getId(), dao.getGlobalDaoPrice(), newFloorPrice);
            return;
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        if (TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode())) {   // && dao.getErc20TokenDecimals() != null
            updateDao.setDaoFloorPrice(new BigDecimal(newFloorPrice).divide(CommonUtil.getPowBigDecimal(dao.getErc20TokenDecimals())));
        } else {
            // 如果为空，默认为18
            updateDao.setDaoFloorPrice(new BigDecimal(newFloorPrice).divide(CommonUtil.getPowBigDecimal(dao.getInputTokenDecimals())));
        }
        updateDao.setCanvasFloorPrice(updateDao.getDaoFloorPrice());
//        if (!updateDao.getDaoFloorPrice().equals(dao.getDaoFloorPrice())) {
//            commonService.updateCanvasPrice(dao);
//        }
        if (!dao.getDaoFloorPrice().equals(updateDao.getDaoFloorPrice())) {
            log.info("[DaoFloorPriceSetChainService] daoId:{} oldFloorPrice:{} newFloorPrice:{} ", dao.getId(), dao.getDaoFloorPrice(), newFloorPrice);
            //更新所有canvas的价格
            commonService.updateCanvasPrice(dao);
//            commonService.updateCanvasPrice(Integer.valueOf(dao.getCurrentRound()), dao.getId());

            Work work = workService.selectLastGenerateWork(dao.getId());
            if (work != null && !updateDao.getDaoFloorPrice().equals(work.getFixedPrice()) && !WorkStatusEnum.CASTED.getStatus().equals(work.getWorkStatus())) {
                work.setFixedPrice(updateDao.getDaoFloorPrice());
                workService.updateById(work);
            }

        } else {
            log.info("[DaoFloorPriceSetChainService] daoId:{} oldFloorPrice equals newFloorPrice:{} ", dao.getId(), newFloorPrice);
        }


        daoService.updateById(updateDao);
    }


}
