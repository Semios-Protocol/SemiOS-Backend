package semios.dex.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.dex.model.dto.response.AssetPoolPriceResDto;
import semios.dex.model.dto.response.Erc20PriceResDto;
import semios.dex.model.entity.LiquidityPriceRecord;

import java.util.List;

/**
 * <p>
 * erc20流通性价格变更记录 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ILiquidityPriceRecordService extends IService<LiquidityPriceRecord> {
    List<Erc20PriceResDto> getOneHourErc20Price(List<String> erc20AddressList, Long searchTime);

    List<Erc20PriceResDto> getHoursErc20Price(String erc20Address, Long startDate, Long endDate);

    List<AssetPoolPriceResDto> getHoursAssetPoolPrice(String erc20Address, Long startDate, Long endDate);

    LiquidityPriceRecord getLatestAssetPoolPrice(String erc20Address);

    LiquidityPriceRecord getLastHoursPriceRecord(String erc20Address, Integer type, Long recordTime);

    LiquidityPriceRecord getLatestSwapPrice(String erc20Address);
}
