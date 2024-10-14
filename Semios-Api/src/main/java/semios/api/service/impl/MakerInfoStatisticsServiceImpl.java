package semios.api.service.impl;


import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import semios.api.mapper.MakerInfoStatisticsMapper;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.entity.Dao;
import semios.api.model.entity.MakerInfoStatistics;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.vo.req.Maker.AnalyticsTokenParam;
import semios.api.model.vo.res.Maker.*;
import semios.api.model.vo.res.TogetherDaoMakerVo;
import semios.api.service.IDaoService;
import semios.api.service.IMakerInfoStatisticsService;
import semios.api.service.IWorkTopupHarvestService;
import semios.api.utils.CommonUtil;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * <p>
 * seed node maker统计信息 服务实现类
 * </p>
 *
 * @author zhyyao
 * @since 2024-09-09
 */
@Slf4j
@Service
public class MakerInfoStatisticsServiceImpl extends ServiceImpl<MakerInfoStatisticsMapper, MakerInfoStatistics> implements IMakerInfoStatisticsService {


    @Autowired
    private MakerInfoStatisticsMapper makerInfoStatisticsMapper;

    @Autowired
    private IDaoService daoService;

    @Autowired
    private IWorkTopupHarvestService workTopupHarvestService;

    @Value("${user_profile_image}")
    private String headImage;

    @Override
    public Result<AnalyticsBalanceVo> analyticsToken(AnalyticsTokenParam analyticsTokenParam) {
        Result<AnalyticsBalanceVo> result = new Result<>();

        if (analyticsTokenParam.getDaoId() == null || analyticsTokenParam.getType() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("param is error");
            return result;
        }

        Dao dao = daoService.getById(analyticsTokenParam.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("dao is not exist");
            return result;
        }

        AnalyticsBalanceVo analyticsBalanceVo = new AnalyticsBalanceVo();
        TogetherDaoMakerVo togetherDaoMakerVo = workTopupHarvestService.getTogetherDaoMakerVo(StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId());

        // type == 1 is input token
        if (TrueOrFalseEnum.TRUE.getStatus().equals(analyticsTokenParam.getType())) {
            analyticsBalanceVo.setTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
            analyticsBalanceVo.setSymbol(dao.getPayCurrencyType());
            analyticsBalanceVo.setTotalToken(new BigDecimal(togetherDaoMakerVo.getNoSpendEthAmount()));
        } else {
            analyticsBalanceVo.setTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
            analyticsBalanceVo.setSymbol(dao.getDaoSymbol());
            analyticsBalanceVo.setTotalToken(new BigDecimal(togetherDaoMakerVo.getNoSpendTokenAmount()));
        }

        analyticsTokenParam.setDaoId(dao.getTogetherDaoId() == null ? dao.getId() : dao.getTogetherDaoId());

        List<AnalyticsTokenDataVo> analyticsTokenDataVoList = makerInfoStatisticsMapper.analyticsTokenData(analyticsTokenParam);
        // 将最后一个值修改为analyticsBalanceVo的total token(将今天的数据进行替换)
        if (!analyticsTokenDataVoList.isEmpty()) {
            analyticsTokenDataVoList.get(analyticsTokenDataVoList.size() - 1).setTokenBalance(analyticsBalanceVo.getTotalToken());
        }
        analyticsBalanceVo.setAnalyticsTokenDataVoList(analyticsTokenDataVoList);
        result.setData(analyticsBalanceVo);

        return result;
    }


    @Override
    public Result<AnalyticsMakerVo> analyticsMaker(AnalyticsTokenParam analyticsTokenParam) {
        Result<AnalyticsMakerVo> result = new Result<>();

        if (analyticsTokenParam.getDaoId() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("param is error");
            return result;
        }

        Dao dao = daoService.getById(analyticsTokenParam.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("dao is not exist");
            return result;
        }

        AnalyticsMakerVo analyticsMakerVo = new AnalyticsMakerVo();
        TogetherDaoMakerVo togetherDaoMakerVo = workTopupHarvestService.getTogetherDaoMakerVo(StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId());

        analyticsMakerVo.setTotalMakers(togetherDaoMakerVo.getMakerTotalAmount());

        analyticsTokenParam.setDaoId(dao.getTogetherDaoId() == null ? dao.getId() : dao.getTogetherDaoId());

        List<AnalyticsMakerDataVo> makerInfoStatistics = makerInfoStatisticsMapper.getMakerInfoStatisticsList(analyticsTokenParam);

        if (!makerInfoStatistics.isEmpty()) {
            makerInfoStatistics.get(makerInfoStatistics.size() - 1).setMakersCount(analyticsMakerVo.getTotalMakers());
        }

        analyticsMakerVo.setAnalyticsMakerDataVos(makerInfoStatistics);

        result.setData(analyticsMakerVo);
        return result;
    }

    @Override
    public Result<MakerOwnerInfoVo> analyticsMakerList(AnalyticsTokenParam analyticsTokenParam) {
        Result<MakerOwnerInfoVo> result = new Result<>();

        if (analyticsTokenParam.getDaoId() == null) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc("param is error");
            return result;
        }

        Dao dao = daoService.getById(analyticsTokenParam.getDaoId());
        if (dao == null) {
            result.setResultCode(ResultDesc.AUTH_ERROR.getResultCode());
            result.setResultDesc("dao is not exist");
            return result;
        }

        MakerOwnerInfoVo makerOwnerInfoVo = new MakerOwnerInfoVo();
        makerOwnerInfoVo.setInputSymbol(dao.getPayCurrencyType());
        makerOwnerInfoVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        makerOwnerInfoVo.setOutputSymbol(dao.getDaoSymbol());
        makerOwnerInfoVo.setOutputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        // together project id
        List<MakerOwnerListVo> makerOwnerListVoList = makerInfoStatisticsMapper.getMakerOwnerList(StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId());

        // 生成随机头像
        for (MakerOwnerListVo makerOwnerListVo : makerOwnerListVoList) {
            if (StringUtils.isBlank(makerOwnerListVo.getAvatarAddress())) {
                Random random = new Random();
                int i = random.nextInt(32) + 1;
                String avatar = String.format(headImage, i);
                makerOwnerListVo.setAvatarAddress(avatar);
            }
        }
        makerOwnerInfoVo.setMakerOwnerListVoList(makerOwnerListVoList.isEmpty() ? new ArrayList<>() : makerOwnerListVoList);

        result.setData(makerOwnerInfoVo);
        return result;
    }

}
