package semios.api.model.vo.req;

import lombok.Data;

/**
 * @description: 用户签名验证参数
 * @author: xiangbin
 * @create: 2022-08-11 18:14
 **/
@Data
public class UserSignatureReqVo extends UserProfileReqVo {

    /**
     * 签名hash
     */
    private String signatureHash;

    /**
     * 签名原文
     */
    private String originalText;
}
