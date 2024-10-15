package semios.api.controller;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.Result;
import semios.api.model.vo.req.Maker.AnalyticsTokenParam;
import semios.api.model.vo.res.Maker.AnalyticsBalanceVo;
import semios.api.model.vo.res.Maker.AnalyticsMakerVo;
import semios.api.model.vo.res.Maker.MakerOwnerInfoVo;
import semios.api.service.IMakerInfoStatisticsService;

import javax.servlet.http.HttpServletRequest;

/**
 * <p>
 * seed node maker统计信息 前端控制器
 * </p>
 *
 * @author zhyyao
 * @since 2024-09-09
 */
@Slf4j
@RestController
@RequestMapping("/dao/maker")
public class MakerInfoStatisticsController {

    @Autowired
    private IMakerInfoStatisticsService makerInfoStatisticsService;

    /**
     * 1.13 maker info input/output 的数据统计图
     */
    @RequestMapping("/analytics/token")
    public Result<AnalyticsBalanceVo> analyticsToken(@RequestBody AnalyticsTokenParam analyticsTokenParam,
                                                     HttpServletRequest request) {
        return makerInfoStatisticsService.analyticsToken(analyticsTokenParam);
    }

    /**
     * 1.13 maker 的数据统计图
     * 只用传daoId
     */
    @RequestMapping("/analytics/makers")
    public Result<AnalyticsMakerVo> analyticsMaker(@RequestBody AnalyticsTokenParam analyticsTokenParam,
                                                   HttpServletRequest request) {
        return makerInfoStatisticsService.analyticsMaker(analyticsTokenParam);
    }

    /**
     * 1.13 maker owner 的前100列表，不做分页
     * 只用传daoId
     */
    @RequestMapping("/owner/list")
    public Result<MakerOwnerInfoVo> analyticsMakerList(@RequestBody AnalyticsTokenParam analyticsTokenParam,
                                                       HttpServletRequest request) {
        return makerInfoStatisticsService.analyticsMakerList(analyticsTokenParam);
    }


}
