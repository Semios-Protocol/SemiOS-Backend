package semios.api.model.vo.res.TopUpReward;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * 1.8 nft list
 *
 * @description: nft list
 * @author: zhyyao
 * @create: 2023-11-21 11:20
 **/
@Data
@Slf4j
public class TopupNftListVo {

    /**
     * daoId
     *
     * @ignore
     */
    private Integer daoId;

    /**
     * project id
     *
     * @mock eaf623688818836f3d4bcde3e5fa3b875a587b48bf16d9a3cef0e95b5342fced
     */
    private String projectId;

    /**
     * workId
     *
     * @ignore
     */
    private Integer workId;

    /**
     * erc721 地址
     *
     * @mock 0x20031867cc2cea0f06a3961bfc5e649880d4d8e0
     */
    private String erc721Token;

    /**
     * tokenId
     */
    private Integer workNumber;
}
