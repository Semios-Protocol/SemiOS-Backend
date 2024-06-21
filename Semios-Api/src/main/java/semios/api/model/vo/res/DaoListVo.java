package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Canvas;
import semios.api.model.entity.Dao;
import semios.api.model.entity.DaoDrbStatistics;
import semios.api.model.entity.Favorites;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.service.ICanvasService;
import semios.api.service.IFavoritesService;
import semios.api.service.IWorkService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;
import java.math.RoundingMode;

/**
 * @description: DAO列表信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class DaoListVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * daoId
     */
    private Integer daoId;

    /**
     * dao project Id
     */
    private String projectId;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * DAO的logo地址
     */
    private String daoLogoUrl;

    /**
     * dao描述
     */
    private String daoDescription;

    /**
     * dao状态 0-未创建1-已创建未开始2-已开始3-已结束    --
     */
    private Integer daoStatus;

    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的logo地址
     */
    private String inputTokenLogo;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * dao symbol
     */
    private String daoSymbol;

    /**
     * project对应的erc20 token地址
     */
    private String daoErc20Address;

    /**
     * DAO里面所有Canvas的最低实时价格
     * 如果DAO下没有canvas则展示DAO创建时设置的地板价    --
     */
    private String price = "0";

    /**
     * dao编号
     */
    private String daoNumber;

    /**
     * dao中canvas数量 --
     */
    private Integer canvasAmount = 0;

    /**
     * dao中work数量   --
     */
    private Integer workAmount = 0;

    /**
     * 收藏数量 --
     */
    private Integer favoriteAmount = 0;

    /**
     * 是否被当前用户收藏    --
     */
    private Boolean favorited = false;

    /**
     * 是否设置了黑白名单
     *
     * @mock false
     */
    private Boolean whiteList = false;

    /**
     * 是否开启了TopUp模式 false-否 true-是
     */
    private Boolean topupMode;

    public static DaoListVo transfer(Dao dao, String userAddress, Boolean isFavorited) {

        DaoListVo daoListVo = new DaoListVo();
        daoListVo.setDaoId(dao.getId());
        daoListVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoListVo.setFavoriteAmount(dao.getFavoriteAmount());
//        daoListVo.setDaoDescription(dao.getDaoDescription());
        if (StringUtils.isBlank(dao.getDaoDescription())) {
            daoListVo.setDaoDescription("");
        } else {
            daoListVo.setDaoDescription(dao.getDaoDescription());
        }
        daoListVo.setDaoName(dao.getDaoName());
        daoListVo.setDaoNumber(dao.getDaoNumber() + "");
        daoListVo.setDaoLogoUrl(dao.getDaoLogoUrl());
        daoListVo.setWhiteList((dao.getCanvasCreatedWhitelist() + dao.getMinterWorksWhitelist() + dao.getCanvasCreatedBlacklist()
                + dao.getMinterWorksBlacklist() + dao.getGlobalMintCap()) + dao.getMintCap() + dao.getErc721MintCap() > 0);
        daoListVo.setTopupMode(Integer.valueOf(1).equals(dao.getTopupMode()));
        if (isFavorited != null && isFavorited) {
            daoListVo.setFavorited(true);
        }
        if (StringUtils.isNotBlank(userAddress)) {
            IFavoritesService favoritesService = SpringBeanUtil.getBean(IFavoritesService.class);
            if (favoritesService != null) {
                Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), dao.getId() + "", userAddress);
                if (favorites != null) {
                    daoListVo.setFavorited(true);
                }
            }
        }
        //单独查询canvas的数量
        Integer canvasAmount = 0;
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        if (canvasService != null) {
            canvasAmount = canvasService.listCanvasAmountByDaoId(dao.getId() + "");
            daoListVo.setCanvasAmount(canvasAmount);
        }
        if (canvasAmount > 0) {
            Canvas canvas = canvasService.listCanvasFloorPriceByDaoId(dao.getId() + "");
            if (canvas != null && canvas.getCurrentPrice() != null) {
                daoListVo.setPrice(canvas.getCurrentPrice().stripTrailingZeros().toPlainString());
            }
        } else {
            daoListVo.setPrice(dao.getDaoFloorPrice().stripTrailingZeros().toPlainString());
        }

        daoListVo.setDaoStatus(dao.getDaoStatus());

        daoListVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoListVo.setInputTokenLogo(dao.getInputTokenLogo());
        daoListVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoListVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoListVo.setDaoSymbol(dao.getDaoSymbol());
        daoListVo.setDaoErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));

        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        if (workService != null) {
            if (!DaoStatusEnum.FINISHED.getStatus().equals(dao.getDaoStatus())) {
                Integer workAmount = workService.selectWorkAmounts(dao.getId() + "");
                daoListVo.setWorkAmount(workAmount);
            } else {
                Integer workAmount = workService.selectNftAmounts(dao.getId() + "");
                daoListVo.setWorkAmount(workAmount);
            }
        }


        return daoListVo;
    }

    public static DaoListVo transferDrbStatistics(DaoDrbStatistics daoDrbStatistics) {
        DaoListVo daoListVo = new DaoListVo();
        daoListVo.setDaoId(daoDrbStatistics.getDaoId());
        daoListVo.setDaoName(daoDrbStatistics.getDaoName());
        daoListVo.setDaoLogoUrl(daoDrbStatistics.getDaoLogourl());
        daoListVo.setDaoDescription(daoDrbStatistics.getDaoDescription());
        daoListVo.setFavorited(daoDrbStatistics.getFavorited());
        daoListVo.setWhiteList(daoDrbStatistics.getWhiteList());
        daoListVo.setTopupMode(Integer.valueOf(1).equals(daoDrbStatistics.getTopupMode()));
        //单独查询canvas的数量
        Integer canvasAmount = 0;
        ICanvasService canvasService = SpringBeanUtil.getBean(ICanvasService.class);
        if (canvasService != null) {
            canvasAmount = canvasService.listCanvasAmountByDaoId(daoDrbStatistics.getDaoId() + "");
            daoListVo.setCanvasAmount(canvasAmount);
            if (canvasAmount > 0) {
                Canvas canvas = canvasService.listCanvasFloorPriceByDaoId(daoDrbStatistics.getDaoId() + "");
                if (canvas != null && canvas.getCurrentPrice() != null) {
                    daoListVo.setPrice(canvas.getCurrentPrice().stripTrailingZeros().toPlainString());
                }
            } else {
                if (daoDrbStatistics.getDaoFloorPrice() != null) {
                    daoListVo.setPrice(daoDrbStatistics.getDaoFloorPrice().setScale(4, RoundingMode.FLOOR).stripTrailingZeros().toPlainString());
                }
            }
        }

        daoListVo.setDaoNumber(daoDrbStatistics.getDaoNumber());


        IWorkService workService = SpringBeanUtil.getBean(IWorkService.class);
        if (workService != null) {
            if (!DaoStatusEnum.FINISHED.getStatus().equals(daoDrbStatistics.getDaoStatus())) {
                Integer workAmount = workService.selectWorkAmounts(daoDrbStatistics.getDaoId() + "");
                daoListVo.setWorkAmount(workAmount);
            } else {
                Integer workAmount = workService.selectNftAmounts(daoDrbStatistics.getDaoId() + "");
                daoListVo.setWorkAmount(workAmount);
            }

        }

        daoListVo.setFavoriteAmount(daoDrbStatistics.getFavoriteAmount());
        daoListVo.setDaoStatus(daoDrbStatistics.getDaoStatus());

        return daoListVo;
    }

    public void setPrice(String price) {
        if (StringUtils.isNotEmpty(price) && !price.equals("null")) {
            this.price = price;
        }
    }

    public void setCanvasAmount(Integer canvasAmount) {
        if (canvasAmount != null) {
            this.canvasAmount = canvasAmount;
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
