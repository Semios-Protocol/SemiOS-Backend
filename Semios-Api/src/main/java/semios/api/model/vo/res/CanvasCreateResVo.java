package semios.api.model.vo.res;

import lombok.Data;

/**
 * 创建canvas返回实体
 */
@Data
public class CanvasCreateResVo {

    /**
     * dao id
     */
    private String projectId;

    /**
     * canvas uri
     */
    private String canvasUri;
}
