package semios.dex.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Select;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20ResDto;
import semios.dex.model.dto.response.Erc20SwapResDto;
import semios.dex.model.entity.Erc20Liquidity;

import java.util.List;

/**
 * <p>
 * erc20流通性表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface Erc20LiquidityMapper extends BaseMapper<Erc20Liquidity> {
    List<Erc20ResDto> getErc20SwapVolumeListBySearchWord(String searchWord, String erc20Address, Long startDate,
                                                         Long endDate, Integer limit);

    List<Erc20BalanceResDto> getErc20TokenListBySearchWord(String searchWord, String erc20Address, String userAddress);

    @Select("select project_id,dao_id,erc20_address,erc721_address,erc20_name,erc20_symbol,erc20_balance,eth_balance,pair_address,eth_address,dao_version\n"
            + "from erc20_liquidity where is_del = 0 and dao_status = 1 and erc20_address = #{erc20Address}")
    Erc20ResDto getErc20Price(String erc20Address);

    Page<Erc20SwapResDto> getErc20SwapVolumeList(IPage<Erc20SwapResDto> page, Long startDate, Long endDate);


    @Select("select * from erc20_liquidity where erc20_address = #{erc20Address}")
    Erc20Liquidity selectErc20LiquidityByErc20Address(String erc20Address);

    @Select("select * from erc20_liquidity where pair_address = #{pairAddress}")
    Erc20Liquidity selectErc20LiquidityByPairAddress(String pairAddress);

    @Select("select * from erc20_liquidity where pair_address is not null and dao_status = 1")
    List<Erc20Liquidity> selectErc20LiquidityStarted();

    @Select("select * from erc20_liquidity where is_del = 0 and dao_status = 1")
    List<Erc20Liquidity> selectAllErc20Liquidity();
}
