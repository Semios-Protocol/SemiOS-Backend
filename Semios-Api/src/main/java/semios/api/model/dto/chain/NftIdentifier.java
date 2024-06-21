package semios.api.model.dto.chain;

import lombok.Data;

/**
 * @description: DesignatedCap
 * @author: xiangbin
 * @create: 2023-04-10 13:50
 **/
@Data
public class NftIdentifier {

    /**
     * struct DesignatedCap { address erc721Address; uint32 tokenId; }
     */

    private String erc721Address;

    private Integer tokenId;

}
