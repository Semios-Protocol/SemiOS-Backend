package semios.api.model.dto.common;

import java.util.List;

/**
 * @author fjtan
 */
public class ResultList<T> {
    private int resultCode = ResultDesc.SUCCESS.getResultCode();
    private String resultDesc = ResultDesc.SUCCESS.getResultDesc();
    private List<T> dataList;
    private Page page;

    public int getResultCode() {
        return resultCode;
    }

    public void setResultCode(int resultCode) {
        this.resultCode = resultCode;
    }

    public String getResultDesc() {
        return resultDesc;
    }

    public void setResultDesc(String resultDesc) {
        this.resultDesc = resultDesc;
    }

    public List<T> getDataList() {
        return dataList;
    }

    public void setDataList(List<T> dataList) {
        this.dataList = dataList;
    }

    public Page getPage() {
        return page;
    }

    public void setPage(Page page) {
        this.page = page;
    }
}
