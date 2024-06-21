package semios.api.model.vo.req;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.vo.RepeatVo;


/**
 * @description: 创建work信息
 * @author: xiangbin
 * @create: 2022-08-10 14:47
 **/
@Data
public class WorkCreateReqVo extends RepeatVo {

    /**
     * dao 的id
     */
    private String daoId;

    /**
     * canvas的Id
     */
    private String canvasId;

    /**
     * work uri 随机数 version_1.5
     */
    private String workUriRandom;

    /**
     * work图片地址
     */
    @JsonIgnore
    private MultipartFile image;

    /**
     * work描述
     */
    private String workDescription;

    /**
     * 创建时的签名hash version_1.5
     */
    private String createSignHash;

    /**
     * 价格类型 0-canvas_price 1-fixed_price version_1.5
     */
    private Integer priceType;

    /**
     * 一口价 version_1.5
     */
    private String fixedPrice;


    /**
     * @ignore
     */
    private String userAddress;


    /**
     * @ignore
     */
    private String imageUrl;

}
