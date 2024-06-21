package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.Serializable;

/**
 * @description: 这一系列DAO的Treasury的信息
 * @author: zhyyao
 * @create: 2024-02-22 14:45
 **/
@Slf4j
@Data
public class TogetherDaoTreasuryInfoVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * treasury的地址
     */
    private String treasuryAddress;

    /**
     * dao的erc20地址
     */
    private String erc20Address;

    /**
     * main dao的project ID
     */
    private String projectId;

    /**
     * main dao的owner address
     */
    private String ownerAddress;
}
