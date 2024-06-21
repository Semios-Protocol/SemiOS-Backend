package semios.dex.model.vo.res;

import lombok.Data;

/**
 * @description: search nft
 * @author: xiangbin
 * @create: 2023-05-12 10:35
 **/
@Data
public class SearchNftResVo {

    /**
     * workId
     */
    private String workId;

    /**
     * work name
     */
    private String workName;

    /**
     * work 描述
     */
    private String workDesc;


    /**
     * work logo
     */
    private String workLogo;
}
