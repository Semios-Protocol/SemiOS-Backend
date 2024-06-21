package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.CanvasDrbStatistics;
import semios.api.model.entity.Favorites;
import semios.api.model.entity.Work;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.service.IFavoritesService;
import semios.api.service.IWorkService;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @description: 搜索canvas返回对象
 * @author: xiangbin
 * @create: 2022-08-05 14:21
 **/
@Data
public class CanvasListResVo {


    /**
     * 最近三个未铸造的work图片，如果没有则展示canvas图片
     */
    private List<String> worksPicture;


    /**
     * canvas ID
     */
    private String canvasId;

    /**
     * canvas名称
     */
    private String canvasName;

    /**
     * canvas描述
     */
    private String canvasDescription;

    /**
     * canvas当前铸造价格
     */
    private Float price = 0.0f;

    /**
     * dao编号
     */
    private String daoNumber;

    /**
     * canvas编号
     */
    private String canvasNumber;

    /**
     * canvas中work数量
     */
    private Integer workAmount = 0;

    /**
     * 收藏数量
     */
    private Integer favoriteAmount = 0;

    /**
     * 2-已开始3-已结束
     */
    private Integer daoStatus;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    public static CanvasListResVo transfer(Canvas canvas, List<Work> workList, String userAddress, Boolean isFavorited) {

        CanvasListResVo canvasListResVo = new CanvasListResVo();
        List<String> workPictures = workList.stream().map(Work::getImageUrl).collect(Collectors.toList());
        int size = workPictures.size();
        while (size < 3) {
            workPictures.add(canvas.getCanvasLogo());
            size++;
        }
        canvasListResVo.setCanvasId(canvas.getId() + "");
        canvasListResVo.setWorksPicture(workPictures);
        canvasListResVo.setCanvasName(canvas.getCanvasName());
        canvasListResVo.setCanvasNumber(canvas.getCanvasNumber() + "");
        canvasListResVo.setCanvasDescription(canvas.getCanvasDescription());
        canvasListResVo.setDaoNumber(canvas.getDaoNumber() + "");
        canvasListResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvas.getCurrentPrice()));
        canvasListResVo.setFavoriteAmount(canvas.getFavoriteAmount());

        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        if (workService != null) {
            Integer workAmount = workService.selectWorkAmountsByCanvasId(canvas.getCanvasId());
            canvasListResVo.setWorkAmount(workAmount);
        }

        if (isFavorited != null && isFavorited) {
            canvasListResVo.setFavorited(true);
        }
        if (StringUtils.isNotBlank(userAddress)) {
            IFavoritesService favoritesService = SpringBeanUtil.getBean(IFavoritesService.class);
            if (favoritesService != null) {
                Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.CANVAS_FAVORITE.getType(), canvas.getId() + "", userAddress);
                if (favorites != null) {
                    canvasListResVo.setFavorited(true);
                }
            }
        }

        canvasListResVo.setDaoStatus(canvas.getDaoStatus());
        return canvasListResVo;

    }

    public static CanvasListResVo transferDrbStatistics(CanvasDrbStatistics canvasDrbStatistics, Canvas canvas, List<Work> workList) {
        CanvasListResVo canvasListResVo = new CanvasListResVo();
        List<String> workPictures = workList.stream().map(Work::getImageUrl).collect(Collectors.toList());
        int size = workPictures.size();
        while (size < 3) {
            workPictures.add(canvasDrbStatistics.getCanvasLogo());
            size++;
        }
        canvasListResVo.setCanvasId(canvasDrbStatistics.getCanId() + "");
        canvasListResVo.setWorksPicture(workPictures);
        canvasListResVo.setCanvasName(canvasDrbStatistics.getCanvasName());
        canvasListResVo.setCanvasDescription(canvasDrbStatistics.getCanvasDescription());
        canvasListResVo.setFavorited(canvasDrbStatistics.getFavorited());
        if (Integer.valueOf(ProtoDaoConstant.CURRENT_ROUND).equals(canvasDrbStatistics.getDrbNumber())) {
            if (canvasDrbStatistics.getMintPrice() != null) {
                canvasListResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getMintPrice()));
            } else {
                canvasListResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getCurrentPrice()));
            }
        } else {
            canvasListResVo.setPrice(ProtoDaoCommonUtil.bigdecimalToFloat(canvasDrbStatistics.getCurrentPrice()));
        }

        canvasListResVo.setDaoNumber(canvasDrbStatistics.getDaoNumber());
        canvasListResVo.setCanvasNumber(canvasDrbStatistics.getCanvasNumber());

        canvasListResVo.setFavoriteAmount(canvasDrbStatistics.getFavoriteAmount());
        canvasListResVo.setDaoStatus(canvasDrbStatistics.getDaoStatus());

        if (canvas == null || !DaoStatusEnum.FINISHED.getStatus().equals(canvas.getDaoStatus())) {
            IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
            if (workService != null) {
                Integer workAmount = workService.selectWorkAmountsByCanvasId(canvasDrbStatistics.getCanvasIdStr());
                canvasListResVo.setWorkAmount(workAmount);
            }
        } else {
            canvasListResVo.setWorkAmount(canvasDrbStatistics.getNft());
        }


        return canvasListResVo;
    }

    public void setCanvasNumber(String canvasNumber) {
        if (StringUtils.isNotBlank(canvasNumber) && !"null".equals(canvasNumber)) {
            this.canvasNumber = canvasNumber;
        }
    }

    public void setPrice(Float price) {
        if (price != null) {
            this.price = price;
        }
    }

    public void setWorkAmount(Integer workAmount) {
        if (workAmount != null) {
            this.workAmount = workAmount;
        }
    }

    public void setFavoriteAmount(Integer favoriteAmount) {
        if (favoriteAmount != null) {
            this.favoriteAmount = favoriteAmount;
        }
    }
}
