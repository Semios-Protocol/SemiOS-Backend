package semios.dex.model.vo.res;

import lombok.Data;

import java.util.List;

/**
 * apy折线图
 *
 * @author: fjtan
 * @create: 2023-05-19 22:14
 **/
@Data
public class ApyFoldLineResVo {

    /**
     * 时间
     *
     * @mock 1684483200, 1684486800
     */
    private List<Long> time;

    /**
     * 虚线时间
     *
     * @mock 1684483200, 1684486800
     */
    private List<Long> dottedTime;

    /**
     * apy
     *
     * @mock 10.01, 20.02
     */
    private List<String> apy;

    /**
     * 虚线apy
     *
     * @mock 10.01, 20.02
     */
    private List<String> dottedApy;

    /**
     * apy最大值
     *
     * @mock 0.02
     */
    private String maxApy = "0";

}
