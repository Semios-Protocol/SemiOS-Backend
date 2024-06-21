package semios.api.model.vo.res;

import lombok.Data;
import semios.api.model.dto.chain.DesignatedCap;
import semios.api.model.dto.chain.DesignatedNftCap;
import semios.api.model.dto.chain.NftIdentifier;

import java.util.ArrayList;
import java.util.List;

/**
 * dao的黑白名单详情
 *
 * @description:
 * @author: xiangbin
 * @create: 2022-08-04 17:03
 **/
@Data
public class DaoWhiteListResVo {

    /**
     * dao id
     */
    private Integer daoId;

    /**
     * dao projectId
     */
    private String projectId;

    /**
     * 创建canvas策略
     */
    private Strategy createCanvas;

    /**
     * mint nft 策略
     */
    private Strategy minting;

    /**
     * Dao状态0-未创建1-已创建未开始2-已开始3-已结束 4-已停机
     */
    private Integer daoStatus;

    /**
     * dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本
     */
    private Integer daoVersion;

    /**
     * 是否为basic dao 1-proto dao 2- basic dao
     */
    private Integer basicDao;

    @Data
    public class Strategy {

        /**
         * 白名单信息
         */
        private WhiteList whiteList;

        /**
         * erc721地址
         */
        private List<String> whiteListedERC721 = new ArrayList<>();

        /**
         * erc721 下的nft白名单 地址
         */
        private List<NftIdentifier> whiteListedERC721Id = new ArrayList<>();

        /**
         * 黑名单列表
         */
        private List<String> blackList;

        /**
         * 高优白名单信息 用户地址
         */
        private List<DesignatedCap> designatedMintCaps = new ArrayList<>();

        /**
         * 高优白名单信息 erc721地址
         */
        private List<DesignatedCap> erc721MintCaps = new ArrayList<>();

        /**
         * 高优白名单信息 erc721下的nft地址
         */
        private List<DesignatedNftCap> erc721MintIdCaps = new ArrayList<>();

        /**
         * 全局铸造上限 仅mint nft 策略有
         */
        private Integer maxMintingAmount = 0;
    }

    @Data
    public class WhiteList {

        /**
         * 原始值
         */
        private List<String> whiteListAddress;

        /**
         * root hash
         */
        private String rootHash;
    }

}
