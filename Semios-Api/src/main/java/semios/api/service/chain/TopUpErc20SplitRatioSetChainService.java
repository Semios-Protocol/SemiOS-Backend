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
import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;

/*
 * 修改eth-token的比例
 * bytes32 daoId,  //main dao project id
 * uint256 defaultEthRatio // 默认eth-token比例*100
 * bytes32[] subDaoIds, //sub dao的 dao id集合数组
 * uint256[] ethRatios //每个sub dao设置的topup eth流向redeem pool比例，和上个数组11对应
 * */
@Slf4j
@Service
public class TopUpErc20SplitRatioSetChainService implements SubscriberChainService {

    @Resource
    private IDaoService daoService;


    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoTopUpEthToRedeemPoolRatioSetChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0)); //dao的project id
        String defaultEthRatio = CommonUtil.hexToTenString(dataList.get(1)); //eth to token 的比例

        int subDaoSize = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(4))));

        String subDaoIds = "";
        for (int i = 1; i <= subDaoSize; i++) {
            subDaoIds += (dataList.get(i + 4));
        }

        String ethRatios = "";
        for (int i = 1; i <= subDaoSize; i++) {
            ethRatios += (dataList.get(i + 4 + 1 + subDaoSize));
        }

        // 修改together dao默认值token-eth
        Dao togetherDao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus());
        if (togetherDao == null) {
            log.error("[TopUpErc20SplitRatioSetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("TopUpErc20SplitRatioSetChainService cannot find dao");
        }
        Dao updateTogetherDao = new Dao();
        updateTogetherDao.setId(togetherDao.getId());

        BigDecimal ratioDefault = new BigDecimal(defaultEthRatio).divide(new BigDecimal("10000"));
        updateTogetherDao.setEthTokenRoyalty(ratioDefault);
        daoService.updateById(updateTogetherDao);


        // 赋值给 非聚合dao的token-eth
        List<String> subDaoProject = CommonUtil.splitBy32Bytes(subDaoIds);
        List<String> subDaoTokenToEthRatios = CommonUtil.splitBy32Bytes(ethRatios);

        for (int i = 0; i < subDaoProject.size(); i++) {
            Dao dao = daoService.getDaoByProjectId(subDaoProject.get(i), DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());
            if (dao == null) {
                log.error("[TopUpErc20SplitRatioSetChainService] dao not find projectId:{}", projectId);
                throw new RuntimeException("TopUpErc20SplitRatioSetChainService cannot find dao");
            }

            Dao updateDao = new Dao();
            updateDao.setId(dao.getId());

            BigDecimal ratio = new BigDecimal(CommonUtil.hexToTenString(subDaoTokenToEthRatios.get(i))).divide(new BigDecimal("10000"));
            updateDao.setEthTokenRoyalty(ratio);

            daoService.updateById(updateDao);
        }
    }
}
