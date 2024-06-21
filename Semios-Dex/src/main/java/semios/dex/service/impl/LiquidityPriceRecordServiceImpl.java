package semios.dex.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import semios.dex.mapper.LiquidityPriceRecordMapper;
import semios.dex.model.dto.response.AssetPoolPriceResDto;
import semios.dex.model.dto.response.Erc20PriceResDto;
import semios.dex.model.entity.LiquidityPriceRecord;
import semios.dex.service.ILiquidityPriceRecordService;

import java.util.List;

/**
 * <p>
 * erc20流通性价格变更记录 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class LiquidityPriceRecordServiceImpl extends ServiceImpl<LiquidityPriceRecordMapper, LiquidityPriceRecord>
        implements ILiquidityPriceRecordService {

    @Autowired
    LiquidityPriceRecordMapper liquidityPriceRecordMapper;

    @Override
    public List<Erc20PriceResDto> getOneHourErc20Price(List<String> erc20AddressList, Long searchTime) {
        return liquidityPriceRecordMapper.getOneHourErc20Price(erc20AddressList, searchTime);
    }

    @Override
    public List<Erc20PriceResDto> getHoursErc20Price(String erc20Address, Long startDate, Long endDate) {
        return liquidityPriceRecordMapper.getHoursErc20Price(erc20Address, startDate, endDate);
    }

    @Override
    public List<AssetPoolPriceResDto> getHoursAssetPoolPrice(String erc20Address, Long startDate, Long endDate) {
        return liquidityPriceRecordMapper.getHoursAssetPoolPrice(erc20Address, startDate, endDate);
    }

    @Override
    public LiquidityPriceRecord getLatestAssetPoolPrice(String erc20Address) {
        return liquidityPriceRecordMapper.getLatestAssetPoolPrice(erc20Address);
    }

    @Override
    public LiquidityPriceRecord getLastHoursPriceRecord(String erc20Address, Integer type, Long recordTime) {
        return liquidityPriceRecordMapper.getLastHoursPriceRecord(erc20Address, type, recordTime);
    }

    @Override
    public LiquidityPriceRecord getLatestSwapPrice(String erc20Address) {
        return liquidityPriceRecordMapper.getLatestSwapPrice(erc20Address);
    }
}
