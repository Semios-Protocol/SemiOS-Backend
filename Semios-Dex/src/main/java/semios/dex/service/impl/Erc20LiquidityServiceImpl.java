package semios.dex.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.dex.mapper.Erc20LiquidityMapper;
import semios.dex.mapper.SubscribeMapper;
import semios.dex.model.dto.response.Erc20BalanceResDto;
import semios.dex.model.dto.response.Erc20ResDto;
import semios.dex.model.dto.response.Erc20SwapResDto;
import semios.dex.model.entity.Erc20Liquidity;
import semios.dex.model.entity.Subscribe;
import semios.dex.model.vo.PageVo;
import semios.dex.service.IErc20LiquidityService;

import java.util.List;

/**
 * <p>
 * erc20流通性表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class Erc20LiquidityServiceImpl extends ServiceImpl<Erc20LiquidityMapper, Erc20Liquidity>
        implements IErc20LiquidityService {

    @Autowired
    private Erc20LiquidityMapper erc20LiquidityMapper;

    @Autowired
    private SubscribeMapper subscribeMapper;

    @Override
    public List<Erc20ResDto> getErc20SwapVolumeListBySearchWord(String searchWord, String erc20Address, Long startDate,
                                                                Long endDate, Integer limit) {
        return erc20LiquidityMapper.getErc20SwapVolumeListBySearchWord(searchWord, erc20Address, startDate, endDate,
                limit);
    }

    @Override
    public List<Erc20BalanceResDto> getErc20TokenListBySearchWord(String searchWord, String erc20Address,
                                                                  String userAddress) {
        return erc20LiquidityMapper.getErc20TokenListBySearchWord(searchWord, erc20Address, userAddress);
    }

    @Override
    public Erc20ResDto getErc20Price(String erc20Address) {
        return erc20LiquidityMapper.getErc20Price(erc20Address);
    }

    @Override
    public IPage<Erc20SwapResDto> getErc20SwapVolumeList(PageVo pageVo, Long startDate, Long endDate) {
        long pageNo = pageVo.getPageNo();
        long pageSize = pageVo.getPageSize();
        Page<Erc20SwapResDto> page;
        if (pageNo > 1) {
            page = new Page<>(pageNo, pageSize, false);
        } else {
            page = new Page<>(pageNo, pageSize);
        }
        return erc20LiquidityMapper.getErc20SwapVolumeList(page, startDate, endDate);
    }

    @Override
    public Erc20Liquidity selectErc20LiquidityByErc20Address(String erc20Address) {
        return erc20LiquidityMapper.selectErc20LiquidityByErc20Address(erc20Address);
    }

    @Override
    public Erc20Liquidity selectErc20LiquidityByPairAddress(String pairAddress) {
        return erc20LiquidityMapper.selectErc20LiquidityByPairAddress(pairAddress);
    }

    @Override
    public List<Erc20Liquidity> selectErc20LiquidityStarted() {
        return erc20LiquidityMapper.selectErc20LiquidityStarted();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int saveErc20LiquidityAndSubscribe(Erc20Liquidity erc20Liquidity, List<Subscribe> subscribeList) {
        int i = erc20LiquidityMapper.updateById(erc20Liquidity);
        if (subscribeList != null && subscribeList.size() > 0) {
            for (Subscribe subscribe : subscribeList) {
                if (subscribe.getId() == null) {
                    i += subscribeMapper.insert(subscribe);
                } else {
                    i += subscribeMapper.updateById(subscribe);
                }
            }

        }
        return i;
    }

    @Override
    public List<Erc20Liquidity> selectAllErc20Liquidity() {
        return erc20LiquidityMapper.selectAllErc20Liquidity();
    }
}
