package semios.api.service;

import com.baomidou.mybatisplus.extension.service.IService;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoAppendTokenLog;

/**
 * <p>
 * dao代币追加记录 服务类
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface IDaoAppendTokenLogService extends IService<DaoAppendTokenLog> {


    int insertDaoAppendTokenLogAndUpdateDao(DaoAppendTokenLog daoAppendTokenLog, Dao dao);

    DaoAppendTokenLog selectByTransactionHash(String transactionHash);
}
