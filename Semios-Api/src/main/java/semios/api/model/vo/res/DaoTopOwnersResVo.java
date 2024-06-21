package semios.api.model.vo.res;

import lombok.Data;

import java.math.BigDecimal;

/**
 * dao top 100 owners
 *
 * @description: analytics
 * @author: xiangbin
 * @create: 2023-04-19 15:14
 **/
@Data
public class DaoTopOwnersResVo {

    /**
     * 用户名称
     *
     * @mock abc
     */
    private String name;

    /**
     * 用户头像地址 或合约的默认头像
     *
     * @mock http://locaohost/abc.jpg
     */
    private String headImg;

    /**
     * 用户地址 ｜ 合约地址
     *
     * @mock 0X1234
     */
    private String address;

    /**
     * 拥有nft数量
     *
     * @mock 5
     */
    private Integer amount;

    /**
     * nft数量占比
     *
     * @mock 0.25
     */
    private BigDecimal ratio;

    /**
     * 是否为合约地址 0-用户地址 1- 合约地址
     *
     * @mock 1
     */
    private Integer isContract = 0;
}
