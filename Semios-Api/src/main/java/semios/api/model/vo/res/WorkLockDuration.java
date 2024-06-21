package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * @description: 当前用户所有nft资产信息
 * @author: zhyyao
 * @create: 2024-1-29
 **/
@Slf4j
@Data
public class WorkLockDuration {

    /**
     * 当前的区块高度
     */
    private String startBlock = "0";

    /**
     * 锁定的小时数转换为区块高度
     * 小时数*3600/12
     */
    private String durationBlock;

}
