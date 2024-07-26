package semios.api.model.vo.res.NodePermission;

import lombok.Data;

@Data
public class CreateNodeId {

    /**
     * dao id,如果为空值说明还未创建成功
     * @mock 1
     */
    private Integer daoId;

    /**
     * work id,如果为空值说明还未创建成功
     * @mock 1
     */
    private Integer workId;


    /**
     * work图片地址
     */
    private String imgUrl;

    /**
     * 背景颜色 宽260，然后缩放后高度，然后加上背景色
     */
    private String bgColor;

    /**
     * nft高度 宽260之后的高度
     */
    private Double height;
}
