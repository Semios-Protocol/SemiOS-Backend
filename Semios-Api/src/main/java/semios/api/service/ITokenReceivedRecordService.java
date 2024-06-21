package semios.api.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import org.apache.ibatis.annotations.Param;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.entity.UserHarvestToken;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 代币领取记录表 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface ITokenReceivedRecordService extends IService<TokenReceivedRecord> {

    int saveTokenReceivedAndUpdateCanvas(TokenReceivedRecord tokenReceivedRecord, Canvas canvas);

    int saveTokenReceivedAndUpdateDao(TokenReceivedRecord tokenReceivedRecord, Dao dao);

    int saveTokenReceivedAndUpdateHarvestTokenList(TokenReceivedRecord tokenReceivedRecord,
                                                   List<UserHarvestToken> userHarvestTokenList);

    int saveOrUpdateTokenReceivedForTransfer(List<TokenReceivedRecord> tokenReceivedRecordList, List<Canvas> canvasList,
                                             List<Dao> daoList, List<UserHarvestToken> userHarvestTokenArray);

    Page<TokenReceivedRecord> pageRecordList(IPage<TokenReceivedRecord> page, String ownerAddress);

    List<TokenReceivedRecord> recordList(String ownerAddress);

    List<TokenReceivedRecord> recordListByTokenType(String projectId, Integer drbNumber, Integer tokenType);

    List<TokenReceivedRecord> recordListForAnalytics(@Param("daoAnalyticsBo") DaoAnalyticsBo daoAnalyticsBo);

    List<TokenReceivedRecord> syncDexForBurn();

    /**
     * 只返回receive_address
     *
     * @param projectId
     * @return
     */
    List<TokenReceivedRecord> recordListByProjectId(String projectId);

    BigDecimal selectDaoBurnAmount(String projectId);


    BigDecimal selectDaoBurnAmountSum(String togetherDaoId);
}
