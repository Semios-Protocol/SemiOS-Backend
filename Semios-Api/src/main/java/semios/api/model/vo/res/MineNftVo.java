package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import semios.api.model.entity.Work;

import java.math.BigDecimal;

/**
 * @description: 当前用户所有nft资产信息
 * @author: zhyyao
 * @create: 2024-1-29
 **/
@Slf4j
@Data
public class MineNftVo {

    /**
     * work id
     *
     * @mock 1
     */
    private Integer workId;

    /**
     * work铸造出来的编号即nft编号
     */
    private Integer workNumber;

    /**
     * work所属的dao的id
     */
    private Integer daoId;

    /**
     * work所属的dao的name
     */
    private String daoName;

    /**
     * work所属的dao编号
     */
    private Integer daoNumber;

    /**
     * work所属的canvas编号
     */
    private Integer canvasNumber;

    /**
     * dao对应的erc721 token address
     */
    private String erc721TokenAddress;

    /**
     * work图片地址
     */
    private String imgUrl;

    /**
     * 背景颜色 宽260，然后缩放后高度，然后加上背景色
     */
    private String bgColor;

    /**
     * nft高度 宽260之后的高度
     */
    private Double height;


    /**
     * nft的erc20余额
     */
    private BigDecimal erc20Amount;

    /**
     * nft的eth余额
     */
    private BigDecimal ethAmount;

    /**
     * Work锁定状态 0-未锁定 1-已锁定
     */
    private Integer workLockStatus;

    /**
     * Work聚合Dao id
     */
    private Integer togetherDaoId;

    /**
     * 聚合dao的daoID
     */
    private String togetherDaoName;


    /**
     * project对应的erc20 token地址
     */
    private String daoErc20Address;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;


    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * 是否开启token 支付模式
     */
    private Integer erc20PaymentMode = 0;


    public static MineNftVo transfor(Work work) {
        return new MineNftVo();
    }
}
