package semios.api.model.vo.res.Maker;

import lombok.Data;

import java.math.BigDecimal;

/*
* node页面的input和output token的数据图表统计
* */
@Data
public class MakerOwnerListVo {
    /**
     * 用户名-可能为空
     * @mock zhyyao
     */
    private String userName;

    /**
     * 用户钱包地址
     * @mock 0x125d349A706f7fE6790ac73b1C32e000A6919b12
     */
    private String userAddress;

    /**
     * 用户头像
     * @mock www.baidu.com
     */
    private String avatarAddress;


    /**
     * input token 余额
     * @mock 123.456
     */
    private BigDecimal inputTokenBalance;

    /**
     * output token 余额
     * @mock 123.456
     */
    private BigDecimal outputTokenBalance;
}
