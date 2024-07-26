package semios.api.model.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * <p>
 * nft权限表
 * </p>
 *
 * @author zhyyao
 * @since 2024-07-16
 */
@TableName("node_permission_nft")
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@Data
public class NodePermissionNft implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * dao id 可能是seed nodes id 或者 sub nodes id
     */
    private Integer daoId;

    /**
     * dao project id 对应nodes的project id
     */
    private String projectId;

    /**
     * node类型 1-seed nodes 2-sub nodes
     */
    private Integer nodeType;

    /**
     * 创建nodes生成的work id
     */
    private Integer generateWorkId;

    /**
     * erc721 address
     */
    private String generateErc721Address;

    /**
     * erc721 token id
     */
    private String generateErc721TokenId;

    /**
     * 权限类型
     * @see semios.api.model.enums.NodePermissionTypeEnum
     */
    private Integer permissionsType;

    /**
     * 这个权限下的nft对应的dao id
     */
    private Integer permissionsDaoId;

    /**
     * 这个权限下的nft对应的work id
     */
    private Integer permissionsWorkId;

    /**
     * erc721 address
     */
    private String permissionsErc721Address;

    /**
     * erc721 token id
     */
    private String permissionsErc721TokenId;

    /**
     * 记录的交易hash
     */
    private String transactionHash;

    /**
     * 是否有效 1-有效 0-无效
     */
    private Boolean isValid;

    /**
     * 是否删除 0-否 1-是
     */
    private Boolean isDel;

    /**
     * 更新时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime updateTime;

    /**
     * 创建时间
     */
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    @JsonSerialize(using = LocalDateTimeSerializer.class)
    private LocalDateTime createTime;
}
