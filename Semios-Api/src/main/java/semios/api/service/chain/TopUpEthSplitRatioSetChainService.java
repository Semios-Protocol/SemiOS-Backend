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
 * 修改token-eth的比例
 * bytes32 daoId,  //main dao project id
 * uint256 defaultEthRatio // 默认eth-token比例*100
 * bytes32[] subDaoIds, //sub dao的 dao id集合数组
 * uint256[] ethRatios //每个sub dao设置的topup eth流向redeem pool比例，和上个数组11对应
 * */
@Slf4j
@Service
public class TopUpEthSplitRatioSetChainService implements SubscriberChainService {

    @Resource
    private IDaoService daoService;

    public static void main(String[] args) throws Exception {
        TransactionDto transactionDto = new TransactionDto();
        transactionDto.setData("0xa62f4a420a9a243ecc0edb0166fe66a1797a03e9d4365e6bc00d653838bc85f30000000000000000000000000000000000000000000000000000000000001e14000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000e00000000000000000000000000000000000000000000000000000000000000002dc5a87171962288191f8581366a36f64e3460f947dc7a11c4b5309208a06abcca62f4a420a9a243ecc0edb0166fe66a1797a03e9d4365e6bc00d653838bc85f3000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000019a6000000000000000000000000000000000000000000000000000000000000154f");
    }

    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[TopUpEthSplitRatioSetChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0)); //dao的project id    // 合约修改字段 defaultInputRatio
        String defaultEthRatio = CommonUtil.hexToTenString(dataList.get(1)); //eth to token 的比例

        int subDaoSize = Integer.parseInt(Objects.requireNonNull(CommonUtil.hexToTenString(dataList.get(4))));  // 合约修改字段 inputRatios

        String subDaoIds = "";
        for (int i = 1; i <= subDaoSize; i++) {
            subDaoIds += (dataList.get(i + 4));
        }

        String ethRatios = "";
        for (int i = 1; i <= subDaoSize; i++) {
            ethRatios += (dataList.get(i + 4 + 1 + subDaoSize));
        }


        // 修改together dao默认值
        Dao togetherDao = daoService.getDaoByProjectId(projectId, DaoTogetherTypeEnum.IS_TOGETHER_DAO.getStatus());
        if (togetherDao == null) {
            log.error("[TopUpEthSplitRatioSetChainService] dao not find projectId:{}", projectId);
            throw new RuntimeException("TopUpEthSplitRatioSetChainService cannot find dao");
        }
        Dao updateTogetherDao = new Dao();
        updateTogetherDao.setId(togetherDao.getId());

        BigDecimal ratioDefault = new BigDecimal(defaultEthRatio).divide(new BigDecimal("10000"));
        updateTogetherDao.setTokenEthRoyalty(ratioDefault);
        daoService.updateById(updateTogetherDao);


        // 赋值给 非聚合dao的sub dao
        List<String> subDaoProject = CommonUtil.splitBy32Bytes(subDaoIds);
        List<String> subDaoEthToTOkenRatios = CommonUtil.splitBy32Bytes(ethRatios);

        for (int i = 0; i < subDaoProject.size(); i++) {
            Dao dao = daoService.getDaoByProjectId(subDaoProject.get(i), DaoTogetherTypeEnum.NOT_TOGETHER_DAO.getStatus());
            if (dao == null) {
                log.error("[TopUpEthSplitRatioSetChainService] dao not find projectId:{}", projectId);
                throw new RuntimeException("TopUpEthSplitRatioSetChainService cannot find dao");
            }

            Dao updateDao = new Dao();
            updateDao.setId(dao.getId());

            BigDecimal ratio = new BigDecimal(CommonUtil.hexToTenString(subDaoEthToTOkenRatios.get(i))).divide(new BigDecimal("10000"));
            updateDao.setTokenEthRoyalty(ratio);

            daoService.updateById(updateDao);
        }

    }


}
