package semios.api.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import semios.api.mapper.CanvasMapper;
import semios.api.mapper.DaoMapper;
import semios.api.mapper.TokenReceivedRecordMapper;
import semios.api.mapper.UserHarvestTokenMapper;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.TokenReceivedRecord;
import semios.api.model.entity.UserHarvestToken;
import semios.api.service.ITokenReceivedRecordService;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 代币领取记录表 服务实现类
 * </p>
 *
 * @author xiangbin
 * @since
 */
@Service
public class TokenReceivedRecordServiceImpl extends ServiceImpl<TokenReceivedRecordMapper, TokenReceivedRecord>
        implements ITokenReceivedRecordService {

    @Autowired
    private TokenReceivedRecordMapper tokenReceivedRecordMapper;

    @Autowired
    private CanvasMapper canvasMapper;

    @Autowired
    private DaoMapper daoMapper;

    @Autowired
    private UserHarvestTokenMapper userHarvestTokenMapper;

    @Transactional
    @Override
    public int saveTokenReceivedAndUpdateCanvas(TokenReceivedRecord tokenReceivedRecord, Canvas canvas) {
        int i = tokenReceivedRecordMapper.insert(tokenReceivedRecord);
        i += canvasMapper.updateById(canvas);
        return i;
    }

    @Transactional
    @Override
    public int saveTokenReceivedAndUpdateDao(TokenReceivedRecord tokenReceivedRecord, Dao dao) {
        int i = tokenReceivedRecordMapper.insert(tokenReceivedRecord);
        i += daoMapper.updateById(dao);
        return i;
    }

    @Override
    public int saveTokenReceivedAndUpdateHarvestTokenList(TokenReceivedRecord tokenReceivedRecord,
                                                          List<UserHarvestToken> userHarvestTokenList) {
        int i = tokenReceivedRecordMapper.insert(tokenReceivedRecord);
        if (userHarvestTokenList != null && userHarvestTokenList.size() > 0) {
            for (UserHarvestToken userHarvestToken : userHarvestTokenList) {
                i += userHarvestTokenMapper.updateById(userHarvestToken);
            }
        }
        return i;
    }

    @Override
    public Page<TokenReceivedRecord> pageRecordList(IPage<TokenReceivedRecord> page, String ownerAddress) {
        return tokenReceivedRecordMapper.pageRecordList(page, ownerAddress);
    }

    @Override
    public List<TokenReceivedRecord> recordList(String ownerAddress) {
        return tokenReceivedRecordMapper.recordList(ownerAddress);
    }

    @Override
    public List<TokenReceivedRecord> recordListByTokenType(String projectId, Integer drbNumber, Integer tokenType) {
        return tokenReceivedRecordMapper.recordListByTokenType(projectId, drbNumber, tokenType);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int saveOrUpdateTokenReceivedForTransfer(List<TokenReceivedRecord> tokenReceivedRecordList,
                                                    List<Canvas> canvasList, List<Dao> daoList, List<UserHarvestToken> userHarvestTokenArray) {
        int i = 0;
        if (tokenReceivedRecordList != null && tokenReceivedRecordList.size() > 0) {
            for (TokenReceivedRecord tokenReceivedRecord : tokenReceivedRecordList) {
                if (tokenReceivedRecord.getId() != null) {
                    i += tokenReceivedRecordMapper.updateById(tokenReceivedRecord);
                } else {
                    i += tokenReceivedRecordMapper.insert(tokenReceivedRecord);
                }
            }
        }
        if (canvasList != null && canvasList.size() > 0) {
            for (Canvas canvas : canvasList) {
                i += canvasMapper.updateById(canvas);
            }
        }
        if (daoList != null && daoList.size() > 0) {
            for (Dao dao : daoList) {
                i += daoMapper.updateById(dao);
            }
        }
        if (userHarvestTokenArray != null && userHarvestTokenArray.size() > 0) {
            for (UserHarvestToken userHarvestToken : userHarvestTokenArray) {
                i += userHarvestTokenMapper.updateById(userHarvestToken);
            }
        }
        return i;
    }

    @Override
    public List<TokenReceivedRecord> recordListForAnalytics(DaoAnalyticsBo daoAnalyticsBo) {
        return tokenReceivedRecordMapper.recordListForAnalytics(daoAnalyticsBo);
    }

    @Override
    public List<TokenReceivedRecord> syncDexForBurn() {
        return tokenReceivedRecordMapper.syncDexForBurn();
    }

    @Override
    public List<TokenReceivedRecord> recordListByProjectId(String projectId) {
        return tokenReceivedRecordMapper.recordListByProjectId(projectId);
    }

    @Override
    public BigDecimal selectDaoBurnAmount(String projectId) {
        return tokenReceivedRecordMapper.selectDaoBurnAmount(projectId);
    }

    @Override
    public BigDecimal selectDaoBurnAmountSum(String togetherDaoId) {
        return tokenReceivedRecordMapper.selectDaoBurnAmountSum(togetherDaoId);
    }
}
