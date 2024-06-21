package semios.api.model.vo.res;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.User;
import semios.api.model.entity.Work;
import semios.api.service.IUserService;
import semios.api.utils.ProtoDaoCommonUtil;
import semios.api.utils.SpringBeanUtil;

/**
 * @description: Rankings下NFTS
 * @author: xiangbin
 * @create: 2022-08-04 18:08
 **/
@Data
public class WorkListForRankingsVo {

    /**
     * work id
     */
    private String workId;
    /**
     * work图片地址
     */
    private String imgUrl;


    /**
     * work所属的dao编号
     */
    private String daoNumber;

    /**
     * work所属的dao名称
     */
    private String daoName;
    /**
     * work所属的canvas编号
     */
    private String canvasNumber;

    /**
     * work铸造出来的编号即nft编号
     */
    private String nftNumber;

    /**
     * work铸造价格
     */
    private Float price;

    /**
     * dao开始时设置的地板价
     */
    private Float floorPrice;

    /**
     * dao的logo
     */
    private String daoLogo;

    /**
     * canvas的logo
     */
    private String canvasLogo;


    /**
     * nft的创建人
     */
    private String creator;

    /**
     * nft的铸造人
     */
    private String minter;

    /**
     * nft的拥有者
     */
    private String owner;

    /**
     * nft的创建人
     */
    private String creatorName;

    /**
     * nft的铸造人
     */
    private String minterName;

    /**
     * nft的拥有者
     */
    private String ownerName;

    /**
     * dao id
     */
    private String daoId;

    /**
     * canvas id
     */
    private String canvasId;

    /**
     * dao 状态 0-未创建1-已创建未开始2-已开始3-已结束
     */
    private Integer daoStatus;

    /**
     * 是否被当前用户收藏
     */
    private Boolean favorited = false;

    /**
     * 是否为一口价 version_1.5
     *
     * @mock false
     */
    private Boolean fixedPrice = false;

    public static WorkListForRankingsVo transfor(Work work) {
        WorkListForRankingsVo workListForRankingsVo = new WorkListForRankingsVo();
        workListForRankingsVo.setWorkId(work.getId() + "");
        workListForRankingsVo.setImgUrl(work.getImageUrl());
        workListForRankingsVo.setDaoNumber(work.getDaoNumber() + "");
        workListForRankingsVo.setDaoName(work.getDaoName());
        workListForRankingsVo.setCanvasNumber(work.getCanvasNumber() + "");
        workListForRankingsVo.setNftNumber(work.getWorkNumber() + "");
        workListForRankingsVo.setPrice(ProtoDaoCommonUtil.bigdecimalToFloat(work.getMintedPrice()));
        workListForRankingsVo.setFloorPrice(ProtoDaoCommonUtil.bigdecimalToFloat(work.getDaoFloorPrice()));
        workListForRankingsVo.setDaoLogo(work.getDaoLogoUrl());
        workListForRankingsVo.setCanvasLogo(work.getCanvasLogo());
        workListForRankingsVo.setCreator(work.getCreatorAddress());
        workListForRankingsVo.setMinter(work.getMintedAddress());
        workListForRankingsVo.setOwner(work.getOwnerAddress());
        workListForRankingsVo.setDaoId(work.getDaoId() + "");
        workListForRankingsVo.setCanvasId(work.getCanId() + "");
        workListForRankingsVo.setDaoStatus(work.getDaoStatus());
        workListForRankingsVo.setFavorited(work.getFavorited());
        workListForRankingsVo.setFixedPrice(work.getFixedPrice() != null);

        IUserService userService = SpringBeanUtil.getBean(IUserService.class);
        if (StringUtils.isNotBlank(work.getCreatorAddress())) {
            if (userService != null) {
                User user = userService.findUserByAddressHash(work.getCreatorAddress().toLowerCase());
                if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                    workListForRankingsVo.setCreatorName(user.getUserName());
                }
            }
        }
        if (StringUtils.isNotBlank(work.getMintedAddress())) {
            if (userService != null) {
                User user = userService.findUserByAddressHash(work.getMintedAddress().toLowerCase());
                if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                    workListForRankingsVo.setMinterName(user.getUserName());
                }
            }
        }
        if (StringUtils.isNotBlank(work.getOwnerAddress())) {
            if (userService != null) {
                User user = userService.findUserByAddressHash(work.getOwnerAddress().toLowerCase());
                if (user != null && StringUtils.isNotBlank(user.getUserName())) {
                    workListForRankingsVo.setOwnerName(user.getUserName());
                }
            }
        }


        return workListForRankingsVo;
    }
}
