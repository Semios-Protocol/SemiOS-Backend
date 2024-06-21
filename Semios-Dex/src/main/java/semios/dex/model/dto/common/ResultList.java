package semios.dex.model.dto.common;

import lombok.Data;

import java.util.List;

/**
 * @author fjtan
 */
@Data
public class ResultList<T> {

    /**
     * 返回状态码
     *
     * @mock 200
     */
    private int resultCode = ResultDesc.SUCCESS.getResultCode();

    /**
     * 返回状态信息
     *
     * @mock 成功
     */
    private String resultDesc = ResultDesc.SUCCESS.getResultDesc();

    /**
     * 返回数据集合
     */
    private List<T> dataList;

    /**
     * 分页信息
     */
    private PageDto page;

}
