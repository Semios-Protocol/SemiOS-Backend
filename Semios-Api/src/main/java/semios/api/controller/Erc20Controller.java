package semios.api.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.api.model.dto.common.Result;
import semios.api.model.entity.Dao;
import semios.api.model.vo.req.Erc20SearchReqVo;
import semios.api.model.vo.res.Erc20SearchResVo;
import semios.api.service.IDaoService;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @description: erc20查询用
 * @author: xiangbin
 * @create: 2023-05-19 15:53
 **/
@Slf4j
@RestController
@RequestMapping("/erc20")
public class Erc20Controller {

    @Autowired
    private IDaoService daoService;

    /**
     * Analytics模块 头部6个统计信息 1.7
     */
    @PostMapping(value = "/statistics")
    public Result<Erc20SearchResVo> searchErc20Info(@RequestBody Erc20SearchReqVo erc20SearchReqVo,
                                                    HttpServletRequest request) {

        Result<Erc20SearchResVo> result = new Result<>();
        if (erc20SearchReqVo == null || erc20SearchReqVo.getErc20Address() == null
                || erc20SearchReqVo.getErc20Address().size() == 0) {
            return result;
        }

        List<Dao> daoList = daoService.selectDaoByErc20TokenList(erc20SearchReqVo.getErc20Address());

        List<Erc20SearchResVo> erc20SearchResVoList =
                daoList.stream().map(Erc20SearchResVo::transfer).collect(Collectors.toList());

        result.setDataList(erc20SearchResVoList);
        return result;

    }
}
