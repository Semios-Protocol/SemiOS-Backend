package semios.api.model.dto.common;

import lombok.Data;

/**
 * @author fjtan
 */
@Data
public class PageDto {
    private long pageNo; // 页码
    private long pageSize; // 每页展示行数
    private String orderBy = null;// 按哪列排序
    private String order = "ASC";// ASC=升序，DESC=降序
    private long count; // 查询结果总数
}
