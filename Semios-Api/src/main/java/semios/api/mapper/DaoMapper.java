package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.vo.req.DaoSortedReqVo;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * dao Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface DaoMapper extends BaseMapper<Dao> {

    // https://blog.csdn.net/panco_/article/details/95201532
    @Select("select count(id) from dao where dao_status > 0  and dao_status != 3 and is_together_dao = 0 and (dao_name like concat('%', #{searchId}, '%') or dao_manitesto like concat('%', #{searchId}, '%') or dao_description like concat('%', #{searchId}, '%') or owner_address like concat('%', #{searchId}, '%'))")
    Integer searchAmount(String searchId);

    @Select("select count(id) from dao where is_together_dao=1 and (dao_name like concat('%', #{searchId}, '%') or dao_manitesto like concat('%', #{searchId}, '%') or dao_description like concat('%', #{searchId}, '%') or owner_address like concat('%', #{searchId}, '%'))")
    Integer searchSeedNodesAmount(String searchId);

    @Select("select dao.*,dds.canvas,dds.works,dds.floor_price, \n" + "                case \n"
            + "                when dao.dao_name like concat('%', #{searchId}, '%') then 3 \n"
            + "                when dao.dao_manitesto like concat('%', #{searchId}, '%') then 2 \n"
            + "                when dao.dao_description like concat('%', #{searchId}, '%') then 1 \n"
            + "                when dao.owner_address like concat('%', #{searchId}, '%') then 4 \n"
            + "                end as ord \n" + "                from dao as dao \n"
            + "                left join ( SELECT MAX( id ) AS id,dao_id FROM dao_drb_statistics GROUP BY dao_id ) as dda on dao.id = dda.dao_id \n"
            + "                left join dao_drb_statistics as dds on dda.id = dds.id  \n"
            + "                where dao.dao_status > 0  and dao.dao_status != 3 and dao.is_together_dao = 0 and (dao.dao_name like concat('%', #{searchId}, '%') or dao.dao_manitesto like concat('%', #{searchId}, '%') or dao.dao_description like concat('%', #{searchId}, '%') or owner_address like concat('%', #{searchId}, '%'))\n"
            + "                order by ord desc,dao.block_time desc")
    List<Dao> searchDao(String searchId);


    @Select("select dao.*,case \n " +
            "when dao.dao_name like concat('%', #{searchId}, '%') then 3\n " +
            "when dao.dao_manitesto like concat('%', #{searchId}, '%') then 2\n " +
            "when dao.dao_description like concat('%', #{searchId}, '%') then 1\n " +
            "when dao.owner_address like concat('%', #{searchId}, '%') then 4\n " +
            "end as ord from dao as dao where dao.is_together_dao = 1 and (dao.dao_name like concat('%', #{searchId}, '%') or dao.dao_manitesto like concat('%', #{searchId}, '%') or dao.dao_description like concat('%', #{searchId}, '%') or owner_address like concat('%', #{searchId}, '%')) order by ord desc,dao.block_time desc\n")
    List<Dao> searchSeedNodes(String searchId);

    @Select("select * from dao where id = #{daoId} and dao_status > 0")
    Dao daoDetailByDaoId(Integer daoId);

    @Select("select * from dao where owner_address = #{ownerAddress} and dao_status > 0 and is_together_dao = 0 order by block_time desc,id desc")
    Page<Dao> myDaoList(IPage<Dao> page, String ownerAddress);

    @Select("select * from dao where owner_address = #{ownerAddress} and dao_status > 0 and is_together_dao = 0 order by block_time desc,id desc")
    List<Dao> myDaoListAll(String ownerAddress);

    Page<Dao> findFavoritesByUserAddress(IPage<Dao> page, String userAddress);

    Page<DaoDrbStatistics> collectionsDao(@Param("page") IPage<DaoDrbStatistics> page,
                                          @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);


    Page<Dao> exploreSeedNodes(@Param("page") IPage<DaoDrbStatistics> page,
                               @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    @Select("select * from dao where project_id = #{projectId} limit 1")
    Dao daoDetailByProjectId(String projectId);

    List<Dao> daoDetailByProjectIdList(List<String> projectIdList);

    @Select("select * from dao where dao_uri = #{daoUri} limit 1")
    Dao daoDetailByUri(String daoUri);

    @Select("select * from dao where dao_status >= 2 and is_together_dao = 0 order by dao_number ")
    List<Dao> daoStarted();

    //    @Select("select * from dao where dao_start_drb <= #{drb} and dao_status = 1")
    @Select("select * from dao where dao_status = 1")
    List<Dao> selectStartDaoByDrb(Integer drb);

    @Select("select * from dao where dao_start_drb + dao_mint_window <= #{drb} and dao_status >= 2 and is_together_dao = 0")
    List<Dao> selectEndedDaoByDrb(Integer drb);

    @Select("select * from dao where LOWER(Replace(dao_name,' ','')) = LOWER(Replace(#{daoName},' ','')) and dao_status != 0 and is_together_dao = 0 limit 1")
    Dao daoDetailByDaoName(String daoName);

    @Select("select * from dao where erc721_token = #{erc721Token} limit 1")
    Dao selectDaoByErc721Token(String erc721Token);

    @Select("select * from dao where erc20_token = #{erc20Token} order by id asc limit 1")
    Dao selectDaoByErc20Token(String erc20Token);

    @Select("select * from dao where dao_status > 0 and is_together_dao = 0 and erc20_token = #{erc20Token} order by id asc")
    List<Dao> selectDaoListByErc20TokenNoPage(String erc20Token);

    @Select("select * from dao where dao_number = #{daoNumber}  and dao_status > 0 and is_together_dao = 0 limit 1")
    Dao selectDaoByDaoNumber(Integer daoNumber);

    @Select("select * from dao where dao_number = #{daoNumber}  and is_together_dao = 1 ")
    Dao selectSeedNodesByDaoNumber(Integer daoNumber);


    List<Dao> selectDaoByDaoNumberList(List<Integer> daoNumberList);

    @Select("select dao_number from dao where dao_number <= #{daoNumber}")
    List<Integer> selectNotAvailableDaoNumber(Integer daoNumber);

    @Select("select * from dao where fresh_opensea = 1")
    List<Dao> freshDaoOpenseaApi();

    @Select("select * from dao where dao_symbol is null or erc20_name is null and dao_status > 0 and is_together_dao = 0")
    List<Dao> freshDaoSymbolAndErc20Name();

    @Select("select * from dao where sync_dex = 1 and is_thirdparty_token = 0")
    List<Dao> syncDexForErc20();

    List<Dao> selectDaoByErc20TokenList(List<String> erc20TokenList);

    @Select("select * from dao where dao_status > 0 and is_together_dao = 0 and transaction_hash = #{transactionHash} ")
    Dao selectDaoByTransactionHash(String transactionHash);

    @Select("select * from dao where splitter_address = #{splitterAddress} ")
    Dao selectDaoBySplitterAddress(String splitterAddress);

    @Update("update dao set royalty_fee_income = #{newValue} where id = #{daoId} and royalty_fee_income = #{originValue}")
    int updateDaoRoyaltyFeeIncome(int daoId, BigDecimal originValue, BigDecimal newValue);

    @Select("select * from dao where dao_status = -1")
    List<Dao> syncDaoStatus();

    @Select("select * from dao where royalty_token_lottery_mode = 1 and dao_status = 2 and is_together_dao = 0")
    List<Dao> selectGenerationMethodDao();


    @Select("select * from dao where dao_status = 2 and is_together_dao = 0 order by dao_name desc")
    List<Dao> protoDaoList();

    @Select("select * from dao where dao_status = 2 and is_together_dao = 0 and add_work = 0 and need_mintable_work = 0 order by dao_name desc")
    List<Dao> protoDaoGenerateList();

    @Select("select sum(cast(erc20_total_supply as signed)) from dao where erc20_token = #{erc20Token}")
    Long selectTotalDaoTokenByErc20Token(String erc20Token);

    //    @Select("select * from dao where erc20_token = #{erc20Token} order by id asc")
    Page<Dao> selectDaoListByErc20Token(@Param("page") IPage<Dao> daoIPage, @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    @Select("select * from dao where (exist_dao_id = #{existDaoId}  or project_id = #{existDaoId}) and dao_status > 0 and is_together_dao = 0 order by block_time desc")
    List<Dao> selectByExistDaoId(String existDaoId);


    @Select("select topup_mode from dao where (exist_dao_id = #{existDaoId}  or project_id = #{existDaoId}) and dao_status > 0 and is_together_dao = 0 and topup_mode = 1 limit 1")
    Boolean selectTopupDaoByExistDaoId(String existDaoId);


    @Select("select * from dao where together_dao_id = #{togetherDaoId} and is_together_dao = 0 order by block_time desc, id desc")
    List<Dao> selectByTogetherDaoId(String togetherDaoId);

    @Select("select * from dao where together_dao_id = #{togetherDaoId} and is_together_dao = 0 order by block_time desc, id desc ")
    Page<Dao> selectByTogetherDaoIdPage(IPage<Dao> daoIPage, String togetherDaoId);

    @Select("select * from dao where dao_status > 0 and is_together_dao = 0 order by block_time desc,id desc")
    List<Dao> listAll();

    @Select("select * from dao where project_id = #{projectId} AND is_together_dao=#{isTogetherDao} limit 1")
    Dao getDaoByProjectId(String projectId, Integer isTogetherDao);

    @Select("select * from dao where id = (select together_dao_id from dao where project_id = #{projectId} AND is_together_dao=0 limit 1)")
    Dao getTogetherDaoBySubDaoProjectId(String projectId);
}
