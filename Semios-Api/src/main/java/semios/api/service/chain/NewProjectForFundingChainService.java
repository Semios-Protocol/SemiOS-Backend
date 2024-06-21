package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.IDaoService;
import semios.api.service.SubscriberChainService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.util.List;

/**
 * 1.3 新建dao抛出事件
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-11-10
 **/
@Slf4j
@Service
public class NewProjectForFundingChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Value("${dao_default_logo}")
    private String daoDefaultLogo;

    public static void main(String[] args) {
        //https://goerli.etherscan.io/tx/0x8ef8593833e0694f81da40af2507f904563855ba7979aeadfa133588a11c3a6f#eventlog
        String data =
                "0xf0919c74cf08b77d5ae71d463d51e2a34ed371cf552e6c0530b09b426a5b833100000000000000000000000000000000000000000000000000000000000000c000000000000000000000000012b4745293c78282ebe8da3984cc0e325257f6c100000000000000000000000058e4aa1807dbdbbf8e0551cfbe613a94e6aabe8a00000000000000000000000000000000000000000000000000000000000002ee0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000005d68747470733a2f2f746573742d70726f746f64616f2e73332e61702d736f757468656173742d312e616d617a6f6e6177732e636f6d2f6d6574612f64616f2f7957564d7971625575326e78754c45537055557736353134312e6a736f6e000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);


        String projectId = dataList.get(0);

        String royaltyFeeRatioInBps = CommonUtil.hexToTenString(dataList.get(5));
        String isAncestorDao = CommonUtil.hexToTenString(dataList.get(6));

        System.out.println("123");
    }

    @Override
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[NewProjectForFundingChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String projectId = dataList.get(0);

        String royaltyFeeRatioInBps = CommonUtil.hexToTenString(dataList.get(4));
        String isAncestorDao = CommonUtil.hexToTenString(dataList.get(5));


        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("NewProjectForFundingChainService cannot find dao");
        }

        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        updateDao.setRoyaltyFee(royaltyFeeRatioInBps);
        updateDao.setIsAncestordao(Integer.valueOf(isAncestorDao));

        Dao togetherDao = null;
        if (TrueOrFalseEnum.TRUE.getStatus().equals(Integer.valueOf(isAncestorDao))) {
            togetherDao = new Dao();
            togetherDao.setProjectId(dao.getProjectId());
            togetherDao.setIsTogetherDao(TrueOrFalseEnum.TRUE.getStatus());
            togetherDao.setDaoName(dao.getDaoName());
//            togetherDao.setDaoNumber(dao.getDaoNumber());
            togetherDao.setDaoLogoUrl(daoDefaultLogo);
            togetherDao.setOwnerAddress(dao.getOwnerAddress());
            togetherDao.setFeePool(dao.getFeePool());
            togetherDao.setErc721Token(dao.getErc721Token());
            togetherDao.setErc20Token(dao.getErc20Token());
            togetherDao.setDaoManitesto(dao.getDaoManitesto());
            togetherDao.setDaoDescription(dao.getDaoDescription());
            togetherDao.setOpenseaLink(dao.getOpenseaLink());
            togetherDao.setDiscordLink(dao.getDiscordLink());
            togetherDao.setTwitterLink(dao.getTwitterLink());
            togetherDao.setSocialLinks(dao.getSocialLinks());
            togetherDao.setDaoStatus(DaoStatusEnum.STARTED.getStatus());
            togetherDao.setDaoRedeemPool(dao.getDaoRedeemPool());

            togetherDao.setPayCurrencyType(dao.getPayCurrencyType());
            togetherDao.setInputTokenAddress(dao.getInputTokenAddress());
            togetherDao.setInputTokenDecimals(dao.getInputTokenDecimals());
            togetherDao.setInputTokenLogo(dao.getInputTokenLogo());
        }


        int togetherDaoId = daoService.insertTogetherDao(togetherDao, updateDao);

        log.info("[NewProjectForFundingChainService] daoId:{} updateDao:{} togetherDaoId:{} ", dao.getId(), JacksonUtil.obj2json(updateDao), togetherDaoId);

    }
}
