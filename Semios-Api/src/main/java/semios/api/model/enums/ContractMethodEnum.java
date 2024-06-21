package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import semios.api.model.dto.common.ProtoDaoConstant;

/**
 * 查询合约或者订阅合约的一些信息
 *
 * @author xiangbin
 */
@NoArgsConstructor
@AllArgsConstructor
public enum ContractMethodEnum {

    PROJECT_INFO(ProtoDaoConstant.protocolContract, "0xe0566833"),
    CANVAS_LAST_PRICE(ProtoDaoConstant.protocolContract, "0x0ceccb05"),
    CANVAS_NEXT_PRICE(ProtoDaoConstant.protocolContract, "0x454b03d1"),
    //function getCanvasNextPrice(bytes32 daoId, bytes32 canvasId) public view returns (uint256)
    GET_CANVAS_NEXT_PRICE(ProtoDaoConstant.protocolContract, "0xf73aaea9"),
    CANVAS_INDEX(ProtoDaoConstant.protocolContract, "0x1f058c5e"), MINT_ADDRESS("", "0x6352211e"),
    CLAIM_CANVAS_REWARD(ProtoDaoConstant.protocolContract, "0x69755dcd"),
    CLAIM_CANVAS_REWARD_FUNDING(ProtoDaoConstant.protocolContract, "0x69755dcd"),
    // CLAIM_PROJECT_ERC20_REWARD(Dao4ArtConstant.protocolContract,"0xd3736e63"),
    FEE_RECEIPT(ProtoDaoConstant.protocolContract, "0x6a1d23e1"), CANVAS_CLAIM("", "0xb69ef8a8"),
    // totalSupply()
    PROJECT_CLAIM(ProtoDaoConstant.protocolContract, "0xd3736e63"),
    // 方法修改 原来是 0xa63ea00e
    CLAIM_DAO_CREATOR_REWARD_FUNDING(ProtoDaoConstant.protocolContract, "0x8c3e6ecc"),
    PROJECT_TOTAL_SUPPLY("", "0x18160ddd"),
    // Transfer(address,address,uint256)
    PROJECT_TRANSFER("", "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),
    CURRENT_ROUND(ProtoDaoConstant.prbContract, "0x8a19c8bc"),
    // protocol - getProjectFloorPrice
    PROJECT_PRICE(ProtoDaoConstant.protocolContract, "0xba0b881b"),
    // erc20 dao symbol
    DAO_SYMBOL("", "0x95d89b41"),
    // erc20 name等
    NAME("", "0x06fdde03"),
    /**
     * ETHTransfered二次交易的版税收益ETHTransfered(address)
     */
    ETH_TRANSFERED("", "0xa1d3396ca9c3273bb83b684217f50dc5ef7ff7d4a33667ff922a9e30692b0bf9"),

    /**
     * 获取 Mint NFT 时 Minter 的 ERC20 分配比例function getNftMinterERC20Ratio(bytes32 daoId) external view returns (uint256);
     */
    GET_NFT_MINTER_ERC_20_RATIO(ProtoDaoConstant.protocolContract, "0xd751f9ff"),
    /**
     * 获取minter可领取的token数量 function claimNftMinterReward(bytes32 daoId, address minter) public returns (uint256)
     */
    CLAIM_NFT_MINTER_REWARD(ProtoDaoConstant.protocolContract, "0xcaadf136"),
    //1.3加
    CLAIM_NFT_MINTER_REWARD_FUNDING(ProtoDaoConstant.protocolContract, "0xcaadf136"),
    GET_REWARD_TILL_ROUND(ProtoDaoConstant.protocolContract, "0xf742b583"),
    GET_ERC_20_REWARD_TILL_ROUND(ProtoDaoConstant.protocolContract, "0x73ab0fcb"),  //unction getOutputRewardTillRound(bytes32 daoId, uint256 round) external view returns (uint256);
    GET_ROUND_REWARD(ProtoDaoConstant.protocolContract, "0x211e16d5"),
    NEXT_DRB_START_BLOCK(ProtoDaoConstant.prbContract, "0x01b2320e"),
    GET_TURNOVER(ProtoDaoConstant.protocolContract, "0x326e8d60"),
    UPDATE_TOP_UP_ACCOUNT(ProtoDaoConstant.protocolContract, "0x86bd41e1"),
    //erc20的balanceOf(address _owner) 调用dao的erc20_token的balanceOf方法，参数为dao的fee_pool地址
    BALANCE_OF("", "0x70a08231"),
    DECIMALS("", "0x313ce567"),
    getDaoERC20PaymentMode(ProtoDaoConstant.protocolContract, "0x234e3552"),//function getDaoOutputPaymentMode(bytes32 daoId) external view returns (bool);   //查询是否开启了erc20支付
    getDaoNeedMintableWork(ProtoDaoConstant.protocolContract, "0x5a0e5d54"),//getDaoNeedMintableWork(bytes32)   //查询是否开启了erc20支付

    getRoundERC20Reward(ProtoDaoConstant.protocolContract, "0x292b5ab7"),//function getRoundOutputReward(bytes32 daoId, uint256 round) external view returns (uint256);
    getRoundETHReward(ProtoDaoConstant.protocolContract, "0x1555a1d0"),// function getRoundInputReward(bytes32 daoId, uint256 round) external view returns (uint256);
    getDaoRemainingRound(ProtoDaoConstant.protocolContract, "0x1a5d996d"),// function getDaoRemainingRound(bytes32 daoId) public view returns (uint256)
    getDaoLastActiveRound(ProtoDaoConstant.protocolContract, "0x38be27c4"),// function getDaoLastActiveRound(bytes32 daoId) public view returns (uint256)
    getDaoLastModifyRound(ProtoDaoConstant.protocolContract, "0x3f927a05"),// function getDaoLastModifyRound(bytes32 daoId)
    GET_DAO_CURRENT_ROUND(ProtoDaoConstant.protocolContract, "0x66ba8699"),

    checkTopUpNftLockedStatus(ProtoDaoConstant.protocolContract, "0xb6014d19"), // 获取锁定状态信息
    UPDATE_WORK_TOP_UP_ACCOUNT(ProtoDaoConstant.protocolContract, "0x9d3e5aeb"), // 获取绑定的余额信息
    GET_PLAN_CURRENT_ROUND(ProtoDaoConstant.protocolContract, "0x3d5d910b"),   // 获取plan的当前周期

    CLAIM_MULTI_PLAN_REWARD(ProtoDaoConstant.protocolContract, "0x96ad109e"),   // 获取plan下nft的奖励数量
    PLAN_CUMULATED_REWARD(ProtoDaoConstant.protocolContract, "0xa7cb27f2"),   // 查询某个plan下到当前周期为止已发放的总奖励数量（包括未领取的）

    GET_TOP_UP_BALANCE(ProtoDaoConstant.protocolContract, "0x13ba0379");   // 查询指定NftIdentifier对应的TopUp账户在具体某个DaoId下的已挂账TopUp余额


    /**
     * 合约地址
     */
    @Getter
    private String contractAddress;

    /**
     * 合约方法
     */
    @Getter
    private String methodAddress;
}
