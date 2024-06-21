package semios.dex.model.dto.common;

import lombok.Data;

/**
 * @author fjtan
 */
@Data
public class PageDto {

    /**
     * 页码
     *
     * @mock 1
     */
    private long pageNo; // 页码

    /**
     * 每页展示行数
     *
     * @mock 10
     */
    private long pageSize; // 每页展示行数

    /**
     * 按哪列排序
     *
     * @mock createTime
     */
    private String orderBy = null;// 按哪列排序

    /**
     * ASC=升序，DESC=降序
     *
     * @mock ASC
     */
    private String order = "ASC";// ASC=升序，DESC=降序

    /**
     * 查询结果总数
     *
     * @mock 100
     */
    private long count; // 查询结果总数
}
