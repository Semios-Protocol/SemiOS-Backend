package semios.dex.model.dto.common;

/**
 * @author fjtan
 */
public class BaseCondition {

    // 页面大小,<=0表示不分页
    private int pageSize = 10;

    // 当前页面
    private int pageNo = 1;

    // 按哪列排序
    private String orderBy = null;

    // ASC=升序，DESC=降序
    private String order = "ASC";

    // 总行数
    private int totalSize = 0;

    private int isCount = 0;

    private int isAll = 0;

    private int offset = 0;

    public int getStart() {
        return ((pageNo - 1) * pageSize) - offset;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public int getPageSize() {
        return pageSize;
    }

    public void setPageSize(int pageSize) {
        this.pageSize = pageSize;
    }

    public int getPageNo() {
        return pageNo;
    }

    public void setPageNo(int pageNo) {
        this.pageNo = pageNo;
    }

    public int getTotalSize() {
        return totalSize;
    }

    public void setTotalSize(int totalSize) {
        this.totalSize = totalSize;
    }

    public int getIsCount() {
        return isCount;
    }

    public void setIsCount(int isCount) {
        this.isCount = isCount;
    }

    public int getIsAll() {
        return isAll;
    }

    public void setIsAll(int isAll) {
        this.isAll = isAll;
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
