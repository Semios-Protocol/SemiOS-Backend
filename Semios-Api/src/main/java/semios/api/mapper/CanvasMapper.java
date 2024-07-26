package semios.api.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.vo.req.DaoSortedReqVo;

import java.util.List;

/**
 * <p>
 * canvas画布 Mapper 接口
 * </p>
 *
 * @author xiangbin
 * @since
 */
public interface CanvasMapper extends BaseMapper<Canvas> {


    @Select("select count(id) from canvas where canvas_status != 0 and (canvas_name like concat('%', #{searchId}, '%') or canvas_description like concat('%', #{searchId}, '%') or canvas_number like concat('%', #{searchId}, '%'))")
    Integer searchAmount(String searchId);

    @Select("select *,\n" +
            "case\n" +
            "when canvas_name like concat('%', #{searchId}, '%') then 3\n" +
            "when canvas_number like concat('%', #{searchId}, '%') then 2\n" +
            "when canvas_description like concat('%', #{searchId}, '%') then 1\n" +
            "end as ord\n" +
            "from canvas where canvas_status != 0 and (canvas_name like concat('%', #{searchId}, '%') or canvas_number like concat('%', #{searchId}, '%') or canvas_description like concat('%', #{searchId}, '%')) order by ord desc,block_time desc")
    List<Canvas> searchCanvas(String searchId);


    @Select("select * from canvas where owner_address = #{ownerAddress} and canvas_status != 0 order by block_time desc,id desc")
    Page<Canvas> myCanvas(IPage<Canvas> page, String ownerAddress);

    @Select("select * from canvas where owner_address = #{ownerAddress} and canvas_status != 0 order by block_time desc,id desc")
    List<Canvas> myCanvasForAll(String ownerAddress);

    @Select("select * from canvas where dao_id = #{daoId} and canvas_status != 0")
    List<Canvas> listCanvasByDaoId(String daoId);


    Page<Canvas> findFavoritesByUserAddress(IPage<Canvas> page, String userAddress);


    Page<CanvasDrbStatistics> collectionsCanvas(@Param("page") IPage<CanvasDrbStatistics> page, @Param("daoSortedReqVo") DaoSortedReqVo daoSortedReqVo);

    @Select("select * from canvas where canvas_id = #{canvasId} and canvas_status != 0")
    Canvas selectCanvasDetailByCanvasId(String canvasId);

    List<Canvas> selectCanvasDetailByCanvasIdList(List<String> canvasIdList);


    @Select("select * from canvas where canvas_uri = #{canvasUri} limit 1")
    Canvas selectCanvasByUri(String canvasUri);


    @Select("select * from canvas where dao_id = #{daoId} and canvas_status != 0 order by current_price asc limit 1")
    Canvas listCanvasFloorPriceByDaoId(String daoId);


    @Select("select count(id) from canvas where dao_id = #{daoId} and canvas_status != 0")
    Integer listCanvasAmountByDaoId(String daoId);

    @Select("select * from canvas where LOWER(Replace(canvas_name,' ','')) = LOWER(Replace(#{canvasName},' ','')) and dao_id = #{daoId} and canvas_status != 0 limit 1")
    Canvas selectCanvasByNameAndDaoId(String canvasName, String daoId);

    @Select("select * from canvas where rest_drb < #{drb} and canvas_status != 0")
    List<Canvas> listCanvasLessThanDrb(Integer drb);

    @Select("select * from canvas where canvas_status != 0 and dao_number = #{daoNumber} and canvas_number = #{canvasNumber}")
    Canvas selectByNumber(Long daoNumber, Long canvasNumber);

    List<Canvas> listCanvasByDaoIds(List<Integer> daoIds);

    @Select("select * from canvas where canvas_status != 0 and transaction_hash = #{transactionHash} limit 1")
    Canvas selectCanvasByTransactionHash(String transactionHash);
}
