package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Work;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.model.enums.WorkPriceTypeEnum;
import semios.api.service.ICanvasService;
import semios.api.service.IDaoService;
import semios.api.service.common.CommonService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.math.BigDecimal;

/**
 * @description: DAO下未铸造的Work列表
 * @author: zhyyao
 * @create: 2024-03-14 11:08
 **/
@Slf4j
@Data
public class WorkListVoV2 {

    /**
     * work id
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

    /**
     * work描述信息
     */
    private String workDescription;

    /**
     * Work状态0-已创建1-已铸造2-已失效
     */
    private Integer workStatus;

    /**
     * work铸造出来的编号即nft编号，未铸造时返回空
     */
    private Integer workNumber;

    /**
     * work所属dao名称
     */
    private String daoName;

    /**
     * work的收藏次数
     */
    private Integer favoriteAmount;


    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * 1.4 是否开启Erc20支付模式 false-否 true-是
     */
    private Boolean erc20PaymentMode = false;

    /**
     * work铸造价格
     */
    private String price = "0";

    /**
     * 1.6.1 价格类型(0-Price)(1-Fixed Price)(2-Unified Price)
     * 0-浮动价格
     * 1-work的一口价
     * 2-dao的全局一口价
     */
    private Integer priceType;


    public static WorkListVoV2 transfor(Work work, Canvas canvas) {
        WorkListVoV2 workListVo = new WorkListVoV2();
        workListVo.setBgColor(work.getBgColor());
        workListVo.setWorkDescription(work.getWorkDescription());
        workListVo.setHeight(work.getHeight());
        // ImageUtil.imageUrlResized
        workListVo.setImgUrl(work.getImageUrl());
        workListVo.setFavoriteAmount(work.getFavoriteAmount());
        workListVo.setFavorited(work.getFavorited());
        workListVo.setWorkStatus(work.getWorkStatus());

        IDaoService daoService = SpringBeanUtil.getBean(IDaoService.class);
        if (daoService == null) {
            log.info("获取到的dao service 为空!!");
            return new WorkListVoV2();
        }
        Dao dao = daoService.getById(work.getDaoId());

        workListVo.setDaoName(dao.getDaoName());
        // 未铸造时展示canvas的价格
        if (work.getMintedPrice() != null) {
            workListVo.setPrice(work.getMintedPrice().stripTrailingZeros().toPlainString());
            workListVo.setWorkNumber(work.getWorkNumber());
        } else {
            if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
                workListVo.setPrice(dao.getGlobalDaoPrice().stripTrailingZeros().toPlainString());
            } else {
                if (canvas == null) {
                    // log.info("[WorkListVo-transfor] canvas is null");
                    ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
                    if (canvasService != null) {
                        log.info("[WorkListVo-transfor] canvasService not null");
                        if (work.getCanId() != null) {
                            canvas = canvasService.getById(work.getCanId());
                        }
                        if (canvas == null && StringUtils.isNotBlank(work.getCanvasId())) {
                            canvas = canvasService.selectCanvasDetailByCanvasId(CommonUtil.removeHexPrefixIfExists(work.getCanvasId()));
                        }
                        if (canvas == null) {
                            canvas = new Canvas();
                            CommonService commonService = SpringBeanUtil.getBean(CommonService.class);
                            if (commonService != null && StringUtils.isNotBlank(work.getCanvasId())) {
                                canvas.setCurrentPrice(commonService.getCanvasNextPrice(dao.getProjectId(), work.getCanvasId(), commonService.getErc20DecimalIfErc20PayModel(dao), dao.getInputTokenDecimals()));
                            } else {
                                canvas.setCurrentPrice(dao.getCanvasFloorPrice());
                            }
                        }
                    }
                }
                if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
                    workListVo.setPrice(work.getFixedPrice().stripTrailingZeros().toPlainString());
                } else {
                    if (canvas != null) {
                        log.info("[WorkListVo-transfor] canvasId:{}", canvas.getId());
                        workListVo.setPrice(canvas.getCurrentPrice().stripTrailingZeros().toPlainString());
                    }
                }
            }
        }

        if (dao.getGlobalDaoPrice() != null && dao.getGlobalDaoPrice().compareTo(BigDecimal.ZERO) >= 0) {
            workListVo.setPriceType(WorkPriceTypeEnum.CANVAS_PRICE.getType());  // 全局一口价类型
        } else if (WorkPriceTypeEnum.FIXED_PRICE.getType().equals(work.getPriceType())) {
            workListVo.setPriceType(WorkPriceTypeEnum.FIXED_PRICE.getType());  // Work一口价类型
        } else {
            workListVo.setPriceType(WorkPriceTypeEnum.DAO_GLOBAL_PRICE.getType());  // 浮动价格类型
        }

        workListVo.setErc20PaymentMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getErc20PaymentMode()));

        workListVo.setWorkId(work.getId());

        return workListVo;
    }
}
