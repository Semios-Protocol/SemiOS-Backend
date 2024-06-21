package semios.dex.model.vo.res;

import lombok.Data;

/**
 * Assets Pool price信息
 *
 * @author: fjtan
 * @create: 2023-05-24 12:14
 **/
@Data
public class AssetPoolPriceResVo {

    /**
     * erc20地址
     *
     * @mock 0x68533e9519f0997b06968dda7f7c9e58b5ba029a
     */
    private String erc20Address;

    /**
     * Asset Pool Price
     *
     * @mock 0.0000012354
     */
    private String assetPoolPrice;

}
