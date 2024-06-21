package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * 追加DaoToken的发放额度
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class DaoTokenSupplySetChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private CommonService commonService;

    public static void main(String[] args) {
        String data = "58d2d3c7a28441df4656fab3eb0e51d94a45b38100a7e74dba50afae6124d6d72ae972c0263f681f4f35a5fbffcb8860ddb2ceb32ab4e3a4d8f6912888dca8c8000000000000000000000000ffd23ddffa1d6181c8a711a8b4939eedf9cc00bd0000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = dataList.get(0);
        String canvasId = dataList.get(1);
        String erc20Token = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(2)));
        String amount = CommonUtil.hexToTenString(dataList.get(3));
        System.out.println("project_id" + " is " + projectId);
        System.out.println("canvas_id" + " is " + canvasId);
        System.out.println("erc20_token" + " is " + erc20Token);
        System.out.println("amount" + " is " + amount);
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[DaoTokenSupplySetChainService] transactionDto:{}", JacksonUtil.obj2json(transactionDto));


        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = CommonUtil.removeHexPrefixIfExists(dataList.get(0));
        String addedDaoToken = CommonUtil.hexToTenString(dataList.get(1));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("DaoTokenSupplySetChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        log.info("[DaoTokenSupplySetChainService] updateDao:{} origin erc20TotalSupply is:{}", dao.getId(), dao.getErc20TotalSupply());
        if (StringUtils.isBlank(dao.getErc20TotalSupply())) {
            updateDao.setErc20TotalSupply(new BigDecimal(addedDaoToken).divide(CommonUtil.getPowBigDecimal(null)).toPlainString()); // dao.getInputTokenDecimals()
        } else {
            updateDao.setErc20TotalSupply(new BigDecimal(dao.getErc20TotalSupply()).add(new BigDecimal(addedDaoToken).divide(CommonUtil.getPowBigDecimal(null))).toPlainString());  // dao.getInputTokenDecimals()
        }
        updateDao.setSubdaoAssetPoolBalance(commonService.erc20BalanceOf(dao.getErc20Token(), dao.getFeePool(), dao.getErc20TokenDecimals(), dao.getInputTokenDecimals()).toPlainString());

        daoService.updateById(updateDao);
        log.info("[DaoTokenSupplySetChainService] updateDao:{} erc20TotalSupply to:{}", dao.getId(), updateDao.getErc20TotalSupply());

    }
}
