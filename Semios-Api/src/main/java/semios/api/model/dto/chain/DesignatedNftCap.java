package semios.api.model.dto.chain;

import lombok.Data;

/**
 * @description: DesignatedCap
 * @author: xiangbin
 * @create: 2023-04-10 13:50
 **/
@Data
public class DesignatedNftCap {

    /**
     * struct DesignatedCap { address account; uint32 cap; }
     */

    private String nftAddress;

    private Integer tokenId;

    private Integer nftMintCap;

}
