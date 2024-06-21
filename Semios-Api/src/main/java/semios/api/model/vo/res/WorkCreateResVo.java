package semios.api.model.vo.res;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @description: work创建时返回信息
 * @author: xiangbin
 * @create: 2022-08-11 14:48
 **/
@Data
public class WorkCreateResVo {

    /**
     * workHash
     */
    public String workHash;
    /**
     * work图片的URL地址
     */
    public String imageUrl;
    /**
     * workId
     */
    private Integer workId;
    /**
     * dao 名称
     */
    private String daoName;
    /**
     * canvas名称
     */
    private String canvasName;
    /**
     * canvas当前价格
     */
    private BigDecimal canvasCurrentPrice;


}
