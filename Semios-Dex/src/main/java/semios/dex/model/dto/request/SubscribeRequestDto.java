package semios.dex.model.dto.request;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 订阅信息
 * @author: xiangbin
 * @create: 2022-04-12 16:45
 **/
@Data
public class SubscribeRequestDto implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 网络类型 Mainnet或者Ropsten
     */
    private String network;

    /**
     * 开始块高度
     */
    private String fromBlock;

    /**
     * 合约地址
     */
    private String address;

    private List<String> topics;//主题数组

    private Integer intervalPeriod;//间隔周期 （秒）

    /**
     * 通知地址
     */
    private String noticeUrl;

    /**
     * 通知类型 0-transaction 1- 数值类型
     *
     * @see
     */
    private Integer noticeType;

    /**
     * appName
     */
    private String appName;

}
