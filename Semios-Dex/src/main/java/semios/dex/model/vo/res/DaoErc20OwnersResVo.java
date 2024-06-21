package semios.dex.model.vo.res;

import lombok.Data;

/**
 * @description: res
 * @author: xiangbin
 * @create: 2023-05-13 10:23
 **/
@Data
public class DaoErc20OwnersResVo {

    /**
     * user address
     *
     * @mock 0x4058b840308df0553b8d15eaf5baa4e40fd7b914
     */
    private String userAddress;

    /**
     * user avatar address
     *
     * @mock https://image.dao4.art/user/19.png
     */
    private String avatarAddress;

    /**
     * 用户名  没有用户名或合约地址时显示地址除0x后大写前六位
     *
     * @mock GibraNia87
     */
    private String userName;

    /**
     * 是否为合约地址 0-否 1-是
     *
     * @mock 0
     */
    private Integer isContract;

    /**
     * erc20数量
     *
     * @mock 100.56
     */
    private String erc20Balance;

    /**
     * erc20数量占总数的百分比
     *
     * @mock 0.1264
     */
    private String percentage;

}
