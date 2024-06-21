package semios.gateway.model.dto.common;

/**
 * @author fjtan
 */
public class Page {
    private long pageNo; // 页码
    private long pageSize; // 每页展示行数
    private String orderBy = null;// 按哪列排序
    private String order = "ASC";// ASC=升序，DESC=降序
    private long count; // 查询结果总数

    public long getPageNo() {
        return pageNo;
    }

    public void setPageNo(long pageNo) {
        this.pageNo = pageNo;
    }

    public long getPageSize() {
        return pageSize;
    }

    public void setPageSize(long pageSize) {
        this.pageSize = pageSize;
    }

    public long getCount() {
        return count;
    }

    public void setCount(long count) {
        this.count = count;
    }

    public String getOrderBy() {
        return orderBy;
    }

    public void setOrderBy(String orderBy) {
        this.orderBy = orderBy;
    }

    public String getOrder() {
        return order;
    }

    public void setOrder(String order) {
        this.order = order;
    }

}
