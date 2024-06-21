package semios.dex.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20ResDto;
import semios.dex.model.dto.response.Erc20SwapResDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.Subscribe;
import semios.dex.model.vo.PageVo;

import java.util.List;

/**
 * <p>
 * erc20流通性表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IErc20LiquidityService extends IService<Erc20Liquidity> {
    List<Erc20ResDto> getErc20SwapVolumeListBySearchWord(String searchWord, String erc20Address, Long startDate,
                                                         Long endDate, Integer limit);

    List<Erc20BalanceResDto> getErc20TokenListBySearchWord(String searchWord, String erc20Address, String userAddress);

    Erc20ResDto getErc20Price(String erc20Address);

    IPage<Erc20SwapResDto> getErc20SwapVolumeList(PageVo pageVo, Long startDate, Long endDate);

    Erc20Liquidity selectErc20LiquidityByErc20Address(String erc20Address);

    Erc20Liquidity selectErc20LiquidityByPairAddress(String pairAddress);

    List<Erc20Liquidity> selectErc20LiquidityStarted();

    int saveErc20LiquidityAndSubscribe(Erc20Liquidity erc20Liquidity, List<Subscribe> subscribeList);

    List<Erc20Liquidity> selectAllErc20Liquidity();
}
