package semios.api.model.vo.res;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.entity.Dao;
import semios.api.utils.CommonUtil;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @description: DAO详情
 * @author: xiangbin
 * @create: 2022-08-04 14:45
 **/
@Slf4j
@Data
public class TogetherDaoDetailVo implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * dao名称
     */
    private String daoName;

    /**
     * dao的logo地址
     */
    private String daoLogoUrl;


    /**
     * dao描述
     */
    private String daoDescription;

    /**
     * dao宣言
     */
    private String daoManitesto;

    /**
     * DAO编号
     */
    private Integer daoNumber;

    /**
     * dao当前状态 0-未开始 1-已开始 2-已结束
     */
    private Integer daoStatus;

    /**
     * dao创建者地址
     */
    private String creatorAddress;


    /**
     * opensea链接地址
     */
    private String openseaLink;

    /**
     * Twitter链接地址
     */
    private String twitterLink;

    /**
     * Discord链接地址
     */
    private String discordLink;


    /**
     * erc20地址
     */
    private String erc20Address;

    /**
     * erc721地址
     */
    private String erc721Address;

    /**
     * feel pool address
     */
    private String feePool;

//    /**
//     * 社交链接
//     * <p>
//     * 社交链接 List<DaoSocialLinks> 的json对象
//     */
//    private List<DaoSocialLinksVo> socialLinks;
    /**
     * 社交链接
     *
     * @mock ["http://123.com","http://456.com"]
     */
    private List<String> socialLinks;

    /**
     * 是否可修改
     *
     * @mock false
     */
    private Boolean modifiable = false;


    /**
     * 1.7 支付货币类型
     */
    private String payCurrencyType;

    /**
     * 1.7 input token的address
     */
    private String inputTokenAddress;

    /**
     * 1.7 work所属的input token的decimals
     */
    private Integer inputTokenDecimals;

    /**
     * dao symbol
     */
    private String daoSymbol;

    public static TogetherDaoDetailVo transfer(Dao dao) {
        TogetherDaoDetailVo daoDetailVo = new TogetherDaoDetailVo();
        daoDetailVo.setDaoId(dao.getId());
        if (StringUtils.isBlank(dao.getDaoDescription())) {
            daoDetailVo.setDaoDescription("");
        } else {
            daoDetailVo.setDaoDescription(dao.getDaoDescription());
        }
        if (StringUtils.isBlank(dao.getDaoManitesto())) {
            daoDetailVo.setDaoManitesto("");
        } else {
            daoDetailVo.setDaoManitesto(dao.getDaoManitesto());
        }
        daoDetailVo.setDaoName(dao.getDaoName());
        daoDetailVo.setDaoLogoUrl(dao.getDaoLogoUrl());
        daoDetailVo.setDiscordLink(StringUtils.isNotBlank(dao.getDiscordLink()) ? dao.getDiscordLink() : "");
        daoDetailVo.setOpenseaLink(StringUtils.isNotBlank(dao.getOpenseaLink()) ? dao.getOpenseaLink() : "");
        daoDetailVo.setTwitterLink(StringUtils.isNotBlank(dao.getTwitterLink()) ? dao.getTwitterLink() : "");
        daoDetailVo.setCreatorAddress(dao.getOwnerAddress());
        daoDetailVo.setErc20Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc20Token()));
        daoDetailVo.setErc721Address(CommonUtil.addHexPrefixIfNotExist(dao.getErc721Token()));

        daoDetailVo.setPayCurrencyType(dao.getPayCurrencyType());
        daoDetailVo.setInputTokenAddress(CommonUtil.addHexPrefixIfNotExist(dao.getInputTokenAddress()));
        daoDetailVo.setInputTokenDecimals(dao.getInputTokenDecimals());
        daoDetailVo.setDaoSymbol(dao.getDaoSymbol());

        if (StringUtils.isNotBlank(dao.getFeePool())) {
            daoDetailVo.setFeePool(CommonUtil.addHexPrefixIfNotExist(dao.getFeePool()));
        }
        if (StringUtils.isNotBlank(dao.getSocialLinks())) {
            daoDetailVo.setSocialLinks(new ArrayList<>(Arrays.asList(dao.getSocialLinks().split(","))));
        } else {
            daoDetailVo.setSocialLinks(Arrays.asList("", "", ""));
        }
        for (int i = 0; i < 3; i++) {
            if (daoDetailVo.getSocialLinks().size() < 3) {
                daoDetailVo.getSocialLinks().add("");
            }
        }

        return daoDetailVo;
    }

    public static void main(String[] args) {

        LocalDate startDate = LocalDate.of(2023, 4, 18);
        long localTime = LocalDateTime.now().toInstant(ZoneOffset.of("+8")).getEpochSecond();
        long startDateTime = startDate.atStartOfDay().plusHours(15).toInstant(ZoneOffset.of("+8")).getEpochSecond();
        System.out.println(startDateTime);
        System.out.println(localTime);
        System.out.println(startDateTime - localTime);
    }
}
