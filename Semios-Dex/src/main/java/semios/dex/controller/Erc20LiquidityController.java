package semios.dex.controller;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import semios.dex.model.dto.common.DexConstant;
import semios.dex.model.dto.common.Result;
import semios.dex.model.dto.common.ResultDesc;
import semios.dex.model.dto.common.ResultList;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20ResDto;
import semios.dex.model.vo.req.Erc20ReqVo;
import semios.dex.model.vo.req.SearchReqVo;
import semios.dex.model.vo.res.SearchNftResVo;
import semios.dex.service.IErc20LiquidityService;
import semios.dex.utils.CommonUtil;
import semios.dex.utils.DateUtil;
import semios.dex.utils.JacksonUtil;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * search 搜索
 *
 * @description: search 搜索
 * @author: xiangbin
 * @create: 2023-05-12 10:32
 **/
@Slf4j
@RestController
@RequestMapping("/liquidity")
public class Erc20LiquidityController extends BaseController {

    @Autowired
    private IErc20LiquidityService erc20LiquidityService;

    /**
     * 导航栏erc20搜索查询接口
     *
     * @param searchReqDto
     * @return
     * @apiNote 搜索框，查询对象为ERC20name、ERC20symbol、ERC20合约地址，最多展示交易量最高五项（返回数据集合dataList）
     */
    @PostMapping("/erc20")
    public ResultList<Erc20ResDto> searchErc20TradingVolume(@RequestBody(required = false) SearchReqVo searchReqDto,
                                                            HttpServletRequest request, HttpServletResponse response) {
        ResultList<Erc20ResDto> result = new ResultList<Erc20ResDto>();
        List<Erc20ResDto> list = new ArrayList<>();
        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, DexConstant.INTERVAL_DAY);
        long endDate = DateUtil.getIntervalTime(today, 1);
        log.info("[searchErc20TradingVolume] searchReqDto:{} startDate:{} endDate:{}", JacksonUtil.obj2json(searchReqDto), startDate, endDate);
        if (searchReqDto == null || StringUtils.isBlank(searchReqDto.getSearchWord())) {
            log.info("[searchErc20TradingVolume] first startDate:{} endDate:{}", startDate, endDate);
            list = erc20LiquidityService.getErc20SwapVolumeListBySearchWord(null, null, startDate, endDate, 5);
        } else {
            String searchWord = searchReqDto.getSearchWord();
            if (CommonUtil.isLetterOrDigit(searchReqDto.getSearchWord())) {
                if (checkAddress(searchWord)) {
                    log.info("[searchErc20TradingVolume] second startDate:{} endDate:{}", startDate, endDate);
                    list = erc20LiquidityService.getErc20SwapVolumeListBySearchWord(null, formatAddress(searchWord),
                            startDate, endDate, 5);
                } else {
                    log.info("[searchErc20TradingVolume] third startDate:{} endDate:{}", startDate, endDate);
                    list = erc20LiquidityService.getErc20SwapVolumeListBySearchWord(searchReqDto.getSearchWord(), null,
                            startDate, endDate, 5);
                }
            }
        }
        result.setDataList(list);
        return result;
    }

    /**
     * Dex默认erc20查询接口
     *
     * @return
     * @apiNote Dex默认erc20查询
     */
    @PostMapping("/default")
    public Result<Erc20ResDto> searchErc20Default(HttpServletRequest request, HttpServletResponse response) {
        Result<Erc20ResDto> result = new Result<Erc20ResDto>();
        Date today = DateUtil.getBeginOfToday();
        long startDate = DateUtil.getIntervalTime(today, DexConstant.INTERVAL_DAY);
        long endDate = DateUtil.getIntervalTime(today, 1);
        List<Erc20ResDto> list =
                erc20LiquidityService.getErc20SwapVolumeListBySearchWord(null, null, startDate, endDate, 1);
        if (list.size() > 0) {
            result.setData(list.get(0));
        }
        return result;
    }

    /**
     * Dex下拉erc20查询接口
     *
     * @return
     * @apiNote 下拉菜单，查询对象为ERC20name、ERC20symbol、ERC20合约地址，按照余额倒序展示全部erc20（返回数据集合dataList）
     */
    @PostMapping("/erc20_token")
    public ResultList<Erc20BalanceResDto> searchErc20Token(@RequestBody(required = false) SearchReqVo searchReqDto,
                                                           HttpServletRequest request, HttpServletResponse response) {
        ResultList<Erc20BalanceResDto> result = new ResultList<Erc20BalanceResDto>();
        List<Erc20BalanceResDto> list;
        if (searchReqDto == null || StringUtils.isBlank(searchReqDto.getSearchWord())) {
            list = erc20LiquidityService.getErc20TokenListBySearchWord(null, null,
                    searchReqDto == null ? null : searchReqDto.getUserAddress());
        } else {
            String searchWord = searchReqDto.getSearchWord();
            if (checkAddress(searchWord)) {
                list = erc20LiquidityService.getErc20TokenListBySearchWord(null, formatAddress(searchWord),
                        searchReqDto.getUserAddress());
            } else {
                list = erc20LiquidityService.getErc20TokenListBySearchWord(searchReqDto.getSearchWord(), null,
                        searchReqDto.getUserAddress());
            }
        }
        result.setDataList(list);
        return result;
    }

    /**
     * Dex指定erc20价格查询接口
     *
     * @return
     * @apiNote Dex指定erc20价格查询
     */
    @PostMapping("/price")
    public Result<Erc20ResDto> searchErc20Price(@RequestBody(required = true) Erc20ReqVo erc20ReqVo,
                                                HttpServletRequest request, HttpServletResponse response) {
        Result<Erc20ResDto> result = new Result<Erc20ResDto>();
        if (!checkErc20ReqVo(erc20ReqVo)) {
            result.setResultCode(ResultDesc.PARAM_ERROR.getResultCode());
            result.setResultDesc(ResultDesc.PARAM_ERROR.getResultDesc() + "wrong erc20 address.");
            return result;
        }
        Erc20ResDto price = erc20LiquidityService.getErc20Price(formatAddress(erc20ReqVo.getErc20Address()));
        if (price == null) {
            return result;
        }
        price.setProjectId(CommonUtil.addHexPrefixIfNotExist(price.getProjectId()));
        result.setData(price);
        return result;
    }

    /**
     * dex 搜索框 NFT编号和NFT说明 最多展示前四项（dataList）
     *
     * @return Result  dataList
     * @ignore
     */
    @Deprecated
    @PostMapping("/nft")
    public ResultList<SearchNftResVo> searchForNft(@RequestBody(required = false) SearchReqVo searchReqDto) {
        ResultList<SearchNftResVo> result = new ResultList<>();

        return result;

    }

    /**
     * 查询系统时间戳（秒）接口
     *
     * @return
     * @apiNote 查询系统时间戳（秒）
     */
    @PostMapping(value = "/times")
    public Result<Long> createDaoTimes() {
        Result<Long> result = new Result<>();
        long currentTime = System.currentTimeMillis() / 1000;
        result.setData(currentTime);

        return result;
    }

}
