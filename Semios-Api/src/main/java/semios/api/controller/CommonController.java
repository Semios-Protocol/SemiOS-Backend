package semios.api.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.vo.res.DrbInfoResVo;
import semios.api.service.common.CommonService;
import semios.api.service.feign.ISubscriptionService;
import semios.api.utils.CommonUtil;
import semios.api.utils.JacksonUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * 通用的查询
 *
 * @description:
 * @author: xiangbin
 * @create: 2023-07-17 15:12
 **/
@Slf4j
@RestController
@RequestMapping("/common")
public class CommonController {

    @Autowired(required = false)
    private ISubscriptionService subscriptionService;

    @Autowired
    private CommonService commonService;


    /**
     * 查询drb时间
     *
     * @return
     */
    @PostMapping(value = "/drb/info")
    public Result<DrbInfoResVo> drbInfo() {
        Result<DrbInfoResVo> resultDrbInfo = new Result<>();
        //获取当前区块，获取总区块数
        Result<String> result = subscriptionService.ethGetBlockNumber(ProtoDaoConstant.netWork);
        if (result.getResultCode() != ResultDesc.SUCCESS.getResultCode()) {
            log.error("[drbInfo] ethGetBlockNumber error:{}", result.getResultDesc());
            resultDrbInfo.setResultCode(ResultDesc.ERROR.getResultCode());
            resultDrbInfo.setResultDesc("network anomaly！ please try again later!");
            return resultDrbInfo;
        }
        String blokcNumber = result.getData();
        String blokcNum = CommonUtil.hexToTenString(blokcNumber);
        log.info("[drbInfo] blockNum:{}", blokcNum);
        if (StringUtils.isBlank(blokcNum)) {
            log.error("[drbInfo] ethGetBlockNumber blokcNum is null");
            resultDrbInfo.setResultCode(ResultDesc.ERROR.getResultCode());
            resultDrbInfo.setResultDesc("network anomaly！ please try again later!");
            return resultDrbInfo;
        }
        //返回对象
        DrbInfoResVo drbInfoResVo = new DrbInfoResVo();

        drbInfoResVo.setProportion(new BigDecimal(String.valueOf(ProtoDaoConstant.NEXT_DRB_START_BLOCK - Integer.parseInt(blokcNum))).
                divide(new BigDecimal(String.valueOf(ProtoDaoConstant.PERIOD_BLOCK)), 2, RoundingMode.HALF_UP).multiply(new BigDecimal("100")).doubleValue());
        //获取下一个drb开始区块
        //获取区块开始时间
        log.info("[drbInfo] NEXT_DRB_START_BLOCK:{}", ProtoDaoConstant.NEXT_DRB_START_BLOCK);
        Double nextDrbStartTime = commonService.nextDrbStartTime();
        if (nextDrbStartTime == null) {
            log.error("[drbInfo] blockTime jsonObject is null");
            resultDrbInfo.setResultCode(ResultDesc.ERROR.getResultCode());
            resultDrbInfo.setResultDesc("network anomaly！ please try again later!");
            return resultDrbInfo;
        }
        drbInfoResVo.setNextPrbStartTime(nextDrbStartTime.intValue());
        log.info("[drbInfo] drbInfoResVo:{}", JacksonUtil.obj2json(drbInfoResVo));
        resultDrbInfo.setData(drbInfoResVo);
        return resultDrbInfo;
    }
}
