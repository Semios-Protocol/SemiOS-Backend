package semios.api.model.vo.res.Maker;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class MakerOwnerInfoVo {
    /**
     * input token 的symbol
     * @mock 1
     */
    private String inputSymbol;

    /**
     * input token 的地址
     * @mock 0x0000000000000000000000000000000000000000
     */
    private String inputTokenAddress;

    /**
     *  output token 的symbol
     * @mock 1
     */
    private String outputSymbol;

    /**
     *  output token 的地址
     * @mock 0x0000000000000000000000000000000000000000
     */
    private String outputTokenAddress;


    /**
     *  owner list 列表
     */
    private List<MakerOwnerListVo> makerOwnerListVoList = new ArrayList<>();

}
