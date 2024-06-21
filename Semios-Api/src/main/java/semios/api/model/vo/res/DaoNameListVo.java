package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.model.entity.Favorites;
import semios.api.model.enums.DaoStatusEnum;
import semios.api.model.enums.FavoriteTypeEnum;
import semios.api.model.enums.TrueOrFalseEnum;
import semios.api.service.IFavoritesService;
import semios.api.utils.CommonUtil;
import semios.api.utils.SpringBeanUtil;

import java.io.Serializable;

/**
 * myDao信息，只返回dao名字
 *
 * @description: DAO列表信息
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Data
public class DaoNameListVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * daoID
     */
    private String daoId;

    /**
     * dao名称
     */
    private String daoName;
    /**
     * dao状态 0-未创建1-已创建未开始2-已开始3-已结束 4-已停机
     */
    private Integer daoStatus;

    /**
     * dao 编号
     */
    private String daoNumber;

    /**
     * 是否停机 0-停机 1-未停机
     */
    private Integer isPaused = 1;

    /**
     * 停机信息提示
     */
    private String pausedMsg;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * dao logo
     */
    private String daoLogo;

    /**
     * 排序字段
     */
    private Integer sorted;

    /**
     * 收藏数量
     */
    private Integer favoriteAmount = 0;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * 是否开启了TopUp模式 0-否 1-是
     */
    private Boolean topupMode;

    /**
     * dao的projectId
     */
    private String projectId;

    /**
     * 是否设置了黑白名单
     *
     * @mock false
     */
    private Boolean whiteList = false;

    public static DaoNameListVo transfer(Dao dao) {
        DaoNameListVo daoNameListVo = new DaoNameListVo();
        daoNameListVo.setDaoId(String.valueOf(dao.getId()));
        daoNameListVo.setSorted(dao.getId());
        daoNameListVo.setDaoName(dao.getDaoName());
        daoNameListVo.setDaoStatus(dao.getDaoStatus());
        daoNameListVo.setDaoVersion(dao.getDaoVersion());
        daoNameListVo.setDaoLogo(dao.getDaoLogoUrl());
        daoNameListVo.setTopupMode(TrueOrFalseEnum.TRUE.getStatus().equals(dao.getTopupMode()));
        daoNameListVo.setProjectId(CommonUtil.addHexPrefixIfNotExist(dao.getProjectId()));
        daoNameListVo.setWhiteList((dao.getCanvasCreatedWhitelist() + dao.getMinterWorksWhitelist()
                + dao.getCanvasCreatedBlacklist() + dao.getMinterWorksBlacklist() + dao.getGlobalMintCap() + dao.getMintCap() + dao.getErc721MintCap()) > 0);
        if (DaoStatusEnum.SHUT_DOWN.getStatus().equals(dao.getDaoStatus())) {
            String msg = "For security reasons, some functions under D4A@%s are temporarily suspended by the DAO.";
            msg = String.format(msg, dao.getDaoNumber());
            daoNameListVo.setPausedMsg(msg);
            daoNameListVo.setIsPaused(0);
        }
        if (dao.getDaoNumber() != null) {
            daoNameListVo.setDaoNumber(dao.getDaoNumber() + "");
        }
        if (StringUtils.isNotBlank(dao.getOwnerAddress())) {
            IFavoritesService favoritesService = SpringBeanUtil.getBean(IFavoritesService.class);
            if (favoritesService != null) {
                Favorites favorites = favoritesService.findByUserAddress(FavoriteTypeEnum.DAO_FAVORITE.getType(), dao.getId() + "", dao.getOwnerAddress());
                if (favorites != null) {
                    daoNameListVo.setFavorited(true);
                    daoNameListVo.setFavoriteAmount(dao.getFavoriteAmount());
                }
            }
        }
        return daoNameListVo;
    }

}
