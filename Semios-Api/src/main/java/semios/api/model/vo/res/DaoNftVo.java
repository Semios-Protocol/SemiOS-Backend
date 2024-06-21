package semios.api.model.vo.res;

import lombok.Data;

import java.io.Serializable;

/**
 * @description: DAO下nft列表
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Data
public class DaoNftVo implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * imgUrl
     */
    private String imgUrl;

    /**
     * 背景颜色 宽260，然后缩放后高度，然后加上背景色
     */
    private String bgColor;


    /**
     * nft高度
     */
    private Double height;


}
