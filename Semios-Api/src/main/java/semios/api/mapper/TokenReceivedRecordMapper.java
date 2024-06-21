package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import semios.api.model.bo.DaoAnalyticsBo;
import semios.api.model.entity.TokenReceivedRecord;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 代币领取记录表 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface TokenReceivedRecordMapper extends BaseMapper<TokenReceivedRecord> {

    @Select("select * from token_received_record where receive_address = #{ownerAddress} or to_address = #{ownerAddress} or from_address = #{ownerAddress} order by block_time desc,id desc")
    Page<TokenReceivedRecord> pageRecordList(IPage<TokenReceivedRecord> page, String ownerAddress);

    @Select("select * from token_received_record where receive_address = #{ownerAddress} or to_address = #{ownerAddress} or from_address = #{ownerAddress} order by block_time desc,id desc")
    List<TokenReceivedRecord> recordList(String ownerAddress);

    @Select("select * from token_received_record where project_id = #{projectId} and drb_number = #{drbNumber} and token_type = #{tokenType} order by block_time desc,id desc")
    List<TokenReceivedRecord> recordListByTokenType(String projectId, Integer drbNumber, Integer tokenType);

    @Select("select * from token_received_record where project_id = #{daoAnalyticsBo.projectId} and DATE_FORMAT(FROM_UNIXTIME(block_time),'%Y-%m-%d') <= #{daoAnalyticsBo.endDate}")
    List<TokenReceivedRecord> recordListForAnalytics(@Param("daoAnalyticsBo") DaoAnalyticsBo daoAnalyticsBo);

    @Select("select * from token_received_record where sync_dex = 0 and token_type = 1")
    List<TokenReceivedRecord> syncDexForBurn();

    @Select("select receive_address from token_received_record where project_id = #{projectId} group by receive_address")
    List<TokenReceivedRecord> recordListByProjectId(String projectId);

    @Select("select sum(token_num) from token_received_record where project_id = #{projectId} and token_type = 1")
    BigDecimal selectDaoBurnAmount(String projectId);


    @Select("select IFNULL(sum(token_num),0) from token_received_record where project_id in (select project_id from dao where together_dao_id = #{togetherDaoId} and is_together_dao = 0) and token_type = 1")
    BigDecimal selectDaoBurnAmountSum(String togetherDaoId);

}
