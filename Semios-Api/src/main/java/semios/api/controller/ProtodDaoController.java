package semios.api.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.dto.common.Result;
import semios.api.model.dto.common.ResultDesc;
import semios.api.model.dto.common.ResultList;
import semios.api.model.entity.Dao;
import semios.api.model.enums.BasicDaoEnum;
import semios.api.model.vo.req.DaoIdReqVo;
import semios.api.model.vo.req.DaoSortedReqVo;
import semios.api.model.vo.res.*;
import semios.api.service.IDaoAllocationStrategyService;
import semios.api.service.IDaoService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.CookieUtil;
import semios.api.utils.JacksonUtil;

import javax.servlet.http.HttpServletRequest;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


/**
 * dao相关接口
 *
 * @author xiangbin
 * @order 1
 */
@Slf4j
@RestController
@RequestMapping("/protodao")
public class ProtodDaoController {

    @Autowired
    private IDaoService daoService;


    @Autowired
    private CommonService commonService;

    @Autowired
    private IDaoAllocationStrategyService daoAllocationStrategyService;


    /**
     * 创建dao查询是否有升级成功的protodao version_1.1
     */
    @PostMapping(value = "/upgrade")
    public ResultList<DaoNameListVo> protodaoUpgrage(HttpServletRequest request) {

        ResultList<DaoNameListVo> result = new ResultList<>();
        result.setDataList(new ArrayList<>());
        String useraddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
        if (StringUtils.isBlank(useraddress)) {
            return result;
        }
        List<Dao> daoList = daoService.myDaoListAll(useraddress);
        if (daoList.isEmpty()) {
            return result;
        }
        //1 创建时间由新到旧 2相同erc20的只展示第一个创建的dao 3 相同erc20的dao 总的发行量不超过1000万
        Map<String, Double> erc20LongMap = daoList.stream().filter(v -> BasicDaoEnum.PROTO_DAO.getBasicType().equals(v.getBasicDao())).collect(Collectors.groupingBy(Dao::getErc20Token, Collectors.summingDouble(v -> Double.parseDouble(v.getErc20TotalSupply()))));

        List<DaoNameListVo> daoNameListVos = daoList.stream().filter(v -> erc20LongMap.containsKey(v.getErc20Token())
                && StringUtils.isBlank(v.getExistDaoId())
                && erc20LongMap.get(v.getErc20Token()) < Double.parseDouble(ProtoDaoConstant.ERC20_TOTAL)).map(DaoNameListVo::transfer).collect(Collectors.toList());
        result.setDataList(daoNameListVos);
        return result;

    }


    /**
     * Related DAOs接口 version_1.1
     * 1.3 只显示当前系列dao的相关dao，如果是另一个dao的三方dao的erc20地址的则不包括进来。
     */
    @PostMapping(value = "/related")
    public ResultList<TogetherDaoListVo> protodaoRelated(@RequestBody(required = false) DaoSortedReqVo daoSortedReqVo, HttpServletRequest request) {
        // 返回值 DaoNameListVo 修改为 TogetherDaoListVo
        ResultList<TogetherDaoListVo> result = new ResultList<>();
        result.setDataList(new ArrayList<>());

        if (daoSortedReqVo == null || StringUtils.isBlank(daoSortedReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoSortedReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        String projectId = StringUtils.isBlank(dao.getExistDaoId()) ? dao.getProjectId() : dao.getExistDaoId();
        IPage<Dao> daoIPage = new Page<>(daoSortedReqVo.getPageNo(), daoSortedReqVo.getPageSize());
        daoSortedReqVo.setErc20Token(dao.getErc20Token());
        daoSortedReqVo.setProjectId(projectId);
        Page<Dao> daoPage = daoService.selectDaoListByErc20Token(daoIPage, daoSortedReqVo);

        List<Dao> daoList = daoPage.getRecords();
        List<DaoNameListVo> daoNameListVos = daoList.stream().filter(v -> v.getProjectId().equalsIgnoreCase(projectId) || projectId.equalsIgnoreCase(v.getExistDaoId())).map(DaoNameListVo::transfer).collect(Collectors.toList());
        log.info("[ProtodDaoController]--原来的值:" + JacksonUtil.obj2json(daoNameListVos));

        if (daoNameListVos.isEmpty()) {
            result.setDataList(new ArrayList<>());
        } else {
            // 1.4.3更改返回值
            List<Integer> daoIdList = daoNameListVos.stream().map(DaoNameListVo::getDaoId).map(Integer::valueOf).collect(Collectors.toList());
            log.info("获取到到id为:" + JacksonUtil.obj2json(daoIdList));

            Map<Integer, Dao> daoMap = daoService.selectDaoByIds(daoIdList)
                    .stream()
                    .collect(Collectors.toMap(Dao::getId, v -> v, (existing, replacement) -> existing, LinkedHashMap::new));
            log.info("获取到到dao类为:" + JacksonUtil.obj2json(daoMap));

            log.info("获取到到id为:" + JacksonUtil.obj2json(daoIdList));
            String userAddress = CookieUtil.getUserAddressFromCookie(request, ProtoDaoConstant.COOKIE_ADDRESS);
            List<TogetherDaoListVo> togetherDaoListVoList = daoIdList.stream()
                    .map(v -> TogetherDaoListVo.transferTogetherDaoListVo(daoMap.get(v), userAddress))
                    .collect(Collectors.toList());

            //List<TogetherDaoListVo> togetherDaoListVoList = daoListTo.stream().map(v-> TogetherDaoListVo.transferTogetherDaoListVo(v,userAddress)).collect(Collectors.toList());

            result.setDataList(togetherDaoListVoList);
        }

        semios.api.model.dto.common.Page page = new semios.api.model.dto.common.Page();
        page.setPageNo(daoSortedReqVo.getPageNo());
        page.setPageSize(daoSortedReqVo.getPageSize());
        page.setCount(daoPage.getTotal());
        result.setPage(page);
        return result;
    }

    /**
     * basic 剩余多少升级到protoDao 返回的为当前流水，需要用2eth减去返回的值
     */
    @PostMapping(value = "/upgrade/flow")
    public Result<DaoFlowResVo> basicUpgrageFlow(@RequestBody(required = false) DaoIdReqVo daoIdReqVo, HttpServletRequest request) {

        Result<DaoFlowResVo> result = new Result<>();
        DaoFlowResVo daoFlowResVo = new DaoFlowResVo();

        if (daoIdReqVo == null || StringUtils.isBlank(daoIdReqVo.getDaoId())) {
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc());
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            return result;
        }
        Dao dao = daoService.daoDetailByDaoId(Integer.valueOf(daoIdReqVo.getDaoId()));
        if (dao == null) {
            result.setResultCode(ResultDesc.NOT_FOUND_ERROR.getResultCode());
            result.setResultDesc("DAO is not exist!");
            return result;
        }
        daoFlowResVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoFlowResVo.setFeePool(dao.getFeePool());
        daoFlowResVo.setBasicDao(dao.getBasicDao());
        if (BasicDaoEnum.PROTO_DAO.getBasicType().equals(dao.getBasicDao())) {
            daoFlowResVo.setDaoFlow(BigDecimal.ZERO);
            result.setData(daoFlowResVo);
            return result;
        }
        if (dao.getDaoFlow() != null) {
            daoFlowResVo.setDaoFlow(new BigDecimal("2").subtract(dao.getDaoFlow()));
        } else {
            daoFlowResVo.setDaoFlow(new BigDecimal("2"));
        }
        result.setData(daoFlowResVo);
        return result;

    }


}
