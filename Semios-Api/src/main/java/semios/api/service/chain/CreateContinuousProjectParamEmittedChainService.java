package semios.api.service.chain;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.request.InfuraCallRequestDto;
import semios.api.model.dto.response.TransactionDto;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.BasicDaoEnum;
import semios.api.model.enums.ContractMethodEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.enums.WorkStatusEnum;
import semios.api.service.IDaoService;
import semios.api.service.IWorkService;
import semios.api.service.SubscriberChainService;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.util.List;

/**
 * proto DAO 扩展信息
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-09-04 13:43
 **/
@Slf4j
@Service
public class CreateContinuousProjectParamEmittedChainService implements SubscriberChainService {

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkService workService;

    @Autowired
    private CommonService commonService;

    @Autowired(required = false)
    private ISubscriptionService iSubscriptionService;

    public static void main(String[] args) {
        //https://goerli.etherscan.io/tx/0x8ef8593833e0694f81da40af2507f904563855ba7979aeadfa133588a11c3a6f#eventlog
        String data =
                "0x000000000000000000000000000000000000000000000000000000000000000058717bcca2312f1cb0dc5ad10138271a0fe13a57c5b157dff6eafc452ddd46d6000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002386f26fc1000000000000000000000000000000000000000000000000000000000000000003e8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String existDaoId = dataList.get(0);
        String projectId = dataList.get(1);
        String dailyMintCap = CommonUtil.hexToTenString(dataList.get(2));
        String needMintableWork = CommonUtil.hexToTenString(dataList.get(3));
        String unifiedPriceModeOff = CommonUtil.hexToTenString(dataList.get(4));
        String unifiedPrice = CommonUtil.hexToTenString(dataList.get(5));
        String reserveNftNumber = CommonUtil.hexToTenString(dataList.get(6));
        String topUpMode = CommonUtil.hexToTenString(dataList.get(7));
        //是否开启无限模式，开启时返回1，关闭时返回0。
        String infiniteMode = CommonUtil.hexToTenString(dataList.get(8));
        //是否开启Erc20支付模式，开启时返回1，关闭时返回0。
        String erc20PaymentMode = CommonUtil.hexToTenString(dataList.get(9));


        System.out.println("projectId" + " is " + projectId);
        System.out.println("existDaoId" + " is " + existDaoId);
        System.out.println("dailyMintCap" + " is " + dailyMintCap);
        System.out.println("needMintableWork" + " is " + needMintableWork);
        System.out.println("unifiedPriceModeOff" + " is " + unifiedPriceModeOff);
        System.out.println("unifiedPrice" + " is " + unifiedPrice);
        System.out.println("reserveNftNumber" + " is " + reserveNftNumber);
        System.out.println("topUpMode" + " is " + topUpMode);
        System.out.println("infiniteMode" + " is " + infiniteMode);
        System.out.println("erc20PaymentMode" + " is " + erc20PaymentMode);
        System.out.println(BigDecimal.ONE.negate().toPlainString());
    }

    public static void main2(String[] args) {
        //https://goerli.etherscan.io/tx/0x8ef8593833e0694f81da40af2507f904563855ba7979aeadfa133588a11c3a6f#eventlog
        String data =
                "0x6d6e29b989aebea8e1ee5dc00f93150c9baad666f2b199c2fbc083c6047f985336a78967d1afef3ee3eb4b73fe2be610cc2e16b9af4f9d8f9440938d7627d8d6000000000000000000000000000000000000000000000000000000000000006400000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002386f26fc1000000000000000000000000000000000000000000000000000000000000000003e8";
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String existDaoId = dataList.get(0);
        String projectId = dataList.get(1);
        String dailyMintCap = CommonUtil.hexToTenString(dataList.get(2));
        String needMintableWork = CommonUtil.hexToTenString(dataList.get(3));
        String unifiedPriceModeOff = CommonUtil.hexToTenString(dataList.get(4));
        String unifiedPrice = CommonUtil.hexToTenString(dataList.get(5));
        String reserveNftNumber = CommonUtil.hexToTenString(dataList.get(6));
        System.out.println("projectId" + " is " + projectId);
        System.out.println("existDaoId" + " is " + existDaoId);
        System.out.println("dailyMintCap" + " is " + dailyMintCap);
        System.out.println("needMintableWork" + " is " + needMintableWork);
        System.out.println("unifiedPriceModeOff" + " is " + unifiedPriceModeOff);
        System.out.println("unifiedPrice" + " is " + unifiedPrice);
        System.out.println("reserveNftNumber" + " is " + reserveNftNumber);
        System.out.println(BigDecimal.ONE.negate().toPlainString());
    }

    @Override
    @Transactional
    public void handleTrade(TransactionDto transactionDto) throws Exception {

        log.info("[CreateContinuousProjectParamEmittedChainService] transactionDao:{}", JacksonUtil.obj2json(transactionDto));

        String data = CommonUtil.removeHexPrefixIfExists(transactionDto.getData());
        List<String> dataList = CommonUtil.splitBy32Bytes(data);

        String existDaoId = dataList.get(0);
        String projectId = dataList.get(1);
        String dailyMintCap = CommonUtil.hexToTenString(dataList.get(2));
        String needMintableWork = CommonUtil.hexToTenString(dataList.get(3));
        String unifiedPriceModeOff = CommonUtil.hexToTenString(dataList.get(4));
        String unifiedPrice = CommonUtil.hexToTenString(dataList.get(5));
        String reserveNftNumber = CommonUtil.hexToTenString(dataList.get(6));
        String topUpMode = CommonUtil.hexToTenString(dataList.get(7));
        //是否开启无限模式，开启时返回1，关闭时返回0。
        String infiniteMode = CommonUtil.hexToTenString(dataList.get(8));
        //是否开启Erc20支付模式，开启时返回1，关闭时返回0。
        String erc20PaymentMode = CommonUtil.hexToTenString(dataList.get(9));

        // 用户输入的input Token地址，为零地址的话默认为Eth
        String inputTokenAddress = CommonUtil.addHexPrefixIfNotExist(CommonUtil.formatBytes32Address(dataList.get(10)));

        Dao dao = daoService.daoDetailByProjectId(projectId);
        if (dao == null) {
            throw new RuntimeException("CreateContinuousProjectParamEmittedChainService cannot find dao");
        }
        Dao updateDao = new Dao();
        updateDao.setId(dao.getId());
        if (!CommonUtil.removeHexPrefixIfExists(ProtoDaoConstant.ZERO_MERKLE_ROOT).equals(existDaoId) && !projectId.equalsIgnoreCase(existDaoId)) {
            updateDao.setExistDaoId(existDaoId);
            updateDao.setBasicDao(BasicDaoEnum.PROTO_DAO.getBasicType());

            Dao existDao = daoService.daoDetailByProjectId(existDaoId);
            if (existDao != null && existDao.getTogetherDaoId() != null) {
                log.info("[CreateContinuousProjectParamEmittedChainService] daoId:{} existDaoId:{} togetherDaoId:{}", dao.getId(), existDaoId, existDao.getTogetherDaoId());
                updateDao.setTogetherDaoId(existDao.getTogetherDaoId());
                updateDao.setInputTokenAddress(existDao.getInputTokenAddress());
                updateDao.setInputTokenDecimals(existDao.getInputTokenDecimals());
                updateDao.setPayCurrencyType(existDao.getPayCurrencyType());
                updateDao.setInputTokenLogo(existDao.getInputTokenLogo());
            }
        }


        if (StringUtils.isNotBlank(dailyMintCap)) {
            updateDao.setDailyMintCap("0".equals(dailyMintCap) ? 10000 : Integer.parseInt(dailyMintCap));
        }
        updateDao.setAddWork("0".equals(needMintableWork) ? 1 : 0);
        updateDao.setNeedMintableWork("0".equals(needMintableWork) ? 1 : 0);
        updateDao.setInfiniteMode("0".equals(infiniteMode) ? 0 : 1);
        updateDao.setErc20PaymentMode("0".equals(erc20PaymentMode) ? 0 : 1);
        updateDao.setRemainingMintWindow(commonService.getDaoRemainingRound(dao).intValue());


        String decimals = commonService.erc20Decimals(dao.getErc20Token());
        String priceData = getProjectPrice(projectId);

        if (StringUtils.isBlank(priceData)) {
            log.error("[CreateContinuousProjectParamEmittedChainService] daoID:{} getProjectPrice error", dao.getId());
            throw new RuntimeException("CreateContinuousProjectParamEmittedChainService update dao err");
        }

        if (StringUtils.isBlank(decimals)) {
            log.error("[CreateContinuousProjectParamEmittedChainService] daoId:{} getDecimals error", dao.getId());
            throw new RuntimeException("CreateContinuousProjectParamEmittedChainService getDecimals err,daoID=" + dao.getId());
        } else {
            if (!new BigDecimal(10).pow(Integer.parseInt(decimals)).toPlainString().equals(ProtoDaoConstant.BASIC_RATIO)) {
                //不等于18位的再记录
                updateDao.setErc20TokenDecimals(Integer.valueOf(decimals));
//                    updateDao.setDaoFloorPrice(dao.getDaoFloorPrice().multiply(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)).divide(new BigDecimal(10).pow(Integer.parseInt(decimals))));
                log.info("[CreateContinuousProjectParamEmittedChainService] updateDaoFloorPrice daoId:{} price:{} newPrice:{}", dao.getId(), dao.getDaoFloorPrice(), updateDao.getDaoFloorPrice());
            }
        }

        // 如果不是零地址，则需要查询input token的decimals 并且 是main dao的话
        if (updateDao.getExistDaoId() == null) {
            if (StringUtils.isNotBlank(inputTokenAddress) && !ProtoDaoConstant.ZERO_ADDRESS.equals(inputTokenAddress)) {
                updateDao.setInputTokenAddress(inputTokenAddress);

                // decimals
                String inputTokenDecimals = commonService.erc20Decimals(inputTokenAddress);
                if (StringUtils.isBlank(inputTokenDecimals)) {
                    log.error("[CreateContinuousProjectParamEmittedChainService] daoId:{} getInputTokenDecimals error", dao.getId());
                    throw new RuntimeException("CreateContinuousProjectParamEmittedChainService getDecimals err,daoID=" + dao.getId());
                }
                updateDao.setInputTokenDecimals(Integer.valueOf(inputTokenDecimals));

                // 获取input token的货币类型
                String inputTokenSymbol = getDaoSymbol(inputTokenAddress);
                updateDao.setPayCurrencyType(inputTokenSymbol);

                //  如何获取input token的logo?
                String inputTokenLogo = getInputTokenLogo(inputTokenAddress);
                updateDao.setInputTokenLogo(inputTokenLogo);
            } else {
                updateDao.setPayCurrencyType(ProtoDaoConstant.DEFAULT_PAY_CURRENCY_TYPE);   // 默认为eth
                updateDao.setInputTokenLogo(ProtoDaoConstant.DEFAULT_PAY_CURRENCY_LOGO);    // eth的话，logo为默认
                updateDao.setInputTokenDecimals(Integer.valueOf(ProtoDaoConstant.BASIC_RATIO_DECIMAL));  // 如果是Eth，则默认18位
            }
        }


        // input token和output token不一样
        // input token如果选了Eth才和之前的一样
        // output token指的是之前的是否开启三方erc20
        //开启erc20支付模式的才需要查询
        if (TrueOrFalseEnum.TRUE.getStatus().equals(updateDao.getErc20PaymentMode())) {
            // 开启erc20支付，使用token支付，使用合约的值除以decimals
            updateDao.setDaoFloorPrice(new BigDecimal(priceData).divide(CommonUtil.getPowBigDecimal(Integer.parseInt(decimals))));
            updateDao.setCanvasFloorPrice(new BigDecimal(priceData).divide(CommonUtil.getPowBigDecimal(Integer.parseInt(decimals))));
        } else {
            // 此方法需要在1.7修改，不能默认除以18次方，用户可能自己输入input token，需要除以input token的decimals
            // 如果没开启erc20支付--将以eth计算，需要除以 18 次方..
            //updateDao.setDaoFloorPrice(new BigDecimal(priceData).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
            //updateDao.setCanvasFloorPrice(new BigDecimal(priceData).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)));
            updateDao.setDaoFloorPrice(new BigDecimal(priceData).divide(CommonUtil.getPowBigDecimal(updateDao.getInputTokenDecimals())));
            updateDao.setCanvasFloorPrice(new BigDecimal(priceData).divide(CommonUtil.getPowBigDecimal(updateDao.getInputTokenDecimals())));
        }


        if ("0".equals(unifiedPriceModeOff)) {
            if (TrueOrFalseEnum.TRUE.getStatus().equals(updateDao.getErc20PaymentMode())) {
                updateDao.setGlobalDaoPrice(new BigDecimal(unifiedPrice).divide(CommonUtil.getPowBigDecimal(updateDao.getErc20TokenDecimals())));
            } else {
                updateDao.setGlobalDaoPrice(new BigDecimal(unifiedPrice).divide(CommonUtil.getPowBigDecimal(updateDao.getInputTokenDecimals())));
            }
            updateDao.setDaoFloorPrice(updateDao.getGlobalDaoPrice());
            updateDao.setCanvasFloorPrice(updateDao.getGlobalDaoPrice());
        } else {
            //-1为关闭
            updateDao.setGlobalDaoPrice(BigDecimal.ONE.negate());
        }
        if (StringUtils.isNotBlank(reserveNftNumber)) {
            updateDao.setGenerateWorkSet(Integer.valueOf(reserveNftNumber));
        }
        updateDao.setTopupMode("0".equals(topUpMode) ? 0 : 1);

        if (!daoService.updateById(updateDao)) {
            log.info("CreateContinuousProjectParamEmittedChainService update dao err:" + JacksonUtil.obj2json(updateDao));
            throw new RuntimeException("CreateContinuousProjectParamEmittedChainService update dao err");
        }
        //如果已经生成了则删掉
        if (updateDao.getNeedMintableWork() == 1) {
            Work work = workService.selectLastGenerateWork(dao.getId());
            if (work != null) {
                log.info("[CreateContinuousProjectParamEmittedChainService] updateDao:{} workId:{}", dao.getId(), work.getId());
                work.setWorkStatus(WorkStatusEnum.EXPIRED.getStatus());
                workService.updateById(work);
            }
        }
        log.info("[CreateContinuousProjectParamEmittedChainService] updateDao:{}  basic dao to proto dao", dao.getId());

    }

    /**
     * 查询 PROJECT_PRICE
     *
     * @param projectId
     * @return
     */
    private String getProjectPrice(String projectId) {
        InfuraCallRequestDto infuraCallPrice = new InfuraCallRequestDto();
        infuraCallPrice.setNetWork(ProtoDaoConstant.netWork);
        infuraCallPrice.setTo(ContractMethodEnum.PROJECT_PRICE.getContractAddress());
        infuraCallPrice.setData(ContractMethodEnum.PROJECT_PRICE.getMethodAddress() + projectId);

        // 调用查询price
        Result<String> resultPrice = iSubscriptionService.infuraCall(infuraCallPrice);
        if (resultPrice.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[CreateProjectParamEmittedFourChainService] price error result:{}", resultPrice.getResultDesc());
            throw new RuntimeException("保存project查询price信息失败");
        }
        log.info("[CreateProjectParamEmittedFourChainService]price infura return data:{}", resultPrice.getData());
        String priceData = resultPrice.getData();

        priceData = CommonUtil.hexToTenString(priceData);
        return priceData;

    }

    /**
     * 查询dao daoSymbol信息
     *
     * @param erc20_token
     * @return
     */
    private String getDaoSymbol(String erc20_token) {
        try {
            InfuraCallRequestDto infuraCallRequestDto2 = new InfuraCallRequestDto();
            infuraCallRequestDto2.setNetWork(ProtoDaoConstant.netWork);
            infuraCallRequestDto2.setTo(erc20_token);
            infuraCallRequestDto2.setData(ContractMethodEnum.DAO_SYMBOL.getMethodAddress());

            // 调用查询使用数据集的user
            Result<String> resul2 = iSubscriptionService.infuraCall(infuraCallRequestDto2);
            if (resul2.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
                log.error("[CreateProjectParamEmittedFourChainService] dao symbol error result:{}", resul2.getResultDesc());
                throw new RuntimeException("保存project查询symbol信息失败");
            }
            log.info("[CreateProjectParamEmittedFourChainService]infura symbol return data:{}", resul2.getData());
            String projectInfoData2 = CommonUtil.removeHexPrefixIfExists(resul2.getData());
            List<String> dataList2 = CommonUtil.splitBy32Bytes(projectInfoData2);
            String daoSymbol = CommonUtil.dynamicArgumentDecoding(projectInfoData2, dataList2.get(0), true);
            return daoSymbol;
        } catch (Exception e) {
            log.error("[CreateProjectParamEmittedFourChainService]infura dao symbol erc20_token:{} exception:{}", erc20_token, e);
        }
        return null;
    }


    private String getInputTokenLogo(String erc20_token) {
        String url = ProtoDaoConstant.etherscanUrl + "/token/" + erc20_token;

        return "test...";
    }

}
