package semios.api.model.vo.req;

import lombok.Data;

/**
 * dao 白名单信息 修改和添加时使用
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoWhiteListReqVo {

    /**
     * @ignore
     */
    private String userAddress;

    /**
     * createCanvas merkle树 proof信息
     */
    private String canvasCreateMerkleProof;

    /**
     * createCanvas 黑白名单地址信息，以逗号分隔
     */
    private String canvasCreateOriginAddress;

    /**
     * createCanvas merkle树 根节点信息
     */
    private String canvasCreateMerkleRoot;


    /**
     * minting merkle树 proof信息
     */
    private String mintingMerkleProof;

    /**
     * minting 黑白名单地址信息，以逗号分隔
     */
    private String mintingOriginAddress;

    /**
     * minting merkle树 根节点信息
     */
    private String mintingMerkleRoot;


}
