package semios.dex.model.vo.req;

import lombok.Data;

/**
 * @description: search
 * @author: xiangbin
 * @create: 2023-05-12 10:35
 **/
@Data
public class SearchReqVo {

    /**
     * 查询参数
     *
     * @mock D4A NFT for No.121|D4A.T124|0x32c3972a564262ad13fbe4bf38ca585f5cead4cb
     */
    private String searchWord;

    /**
     * 用户钱包地址
     *
     * @mock 0x56dBa60a326C8a1e1ED148486A2695884Aa34e3b
     */
    private String userAddress;
}
