package semios.api.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import semios.api.model.dto.common.ProtoDaoConstant;
import semios.api.utils.CommonUtil;

import java.math.BigDecimal;
import java.util.List;

@Slf4j
@NoArgsConstructor
@AllArgsConstructor
public enum TradeTypeEnum {
    MINT_D4A_FEE_RATIO("mintD4aFeeRatio", "nft铸造时d4a协议收费的比例", true, SubscriberTypeEnum.VALUE, "", ""),
    CURRENT_ROUND("currentRound", "当前DRB", false, SubscriberTypeEnum.VALUE, "", ""),
    CURRENT_ROUND_PROJECT("currentRoundProject", "当前DRB", false, SubscriberTypeEnum.VALUE, "", ""),

    PROTOCOL_FEE_POOL("protocol_fee_pool", "feePoolAddress", true, SubscriberTypeEnum.VALUE, "", ""),
    CURRENT_ROUND_LOCAL("currentRoundLocal", "当前DRB", true, SubscriberTypeEnum.VALUE, "", ""),
    MINT_PROJECT_FEE_RATIO("mintPprojectFeeRatio", "nft铸造时project创建者收费的比例", true, SubscriberTypeEnum.VALUE, "", ""),
    MINT_PROJECT_FEE_RATIO_FLAT_PRICE("mintProjectFeeRatioFlatPrice", "一口价nft铸造时project创建者收费的比例", true,
            SubscriberTypeEnum.VALUE, "", ""),
    RATIO_BASE("ratioBase", "比例的base即分母部分", true, SubscriberTypeEnum.VALUE, "", ""),
    // NEW_PROJECT("newProject", "创建project", false, SubscriberTypeEnum.EVENT, "newProjectChainService","0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693"),
    NEW_CANVAS("newCanvas", "创建canvas", false, SubscriberTypeEnum.EVENT, "newCanvasChainService", "0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686"),
    D4A_MINTNFT("d4AMintNFT", "铸造NFT", false, SubscriberTypeEnum.EVENT, "d4AMintNFTChainService", "0x1ce7903c69a8018cfe39951cc5e4afa327100389f4991ed7a6aa8f4083a6e33d"),
    TRANSFER("transfer", "二次交易", false, SubscriberTypeEnum.EVENT, "transferChainService", ""),
    TRANSFER_ERC20("transferErc20", "erc20交易", false, SubscriberTypeEnum.BATCH_TRAN, "transferErc20ChainService", ""),
    // D4A_CLAIM_PROJECT_ERC20_REWARD("d4AClaimProjectERC20Reward", "领取dao的代币", false, SubscriberTypeEnum.EVENT, "d4AClaimProjectERC20RewardChainService"),
    // D4A_CLAIM_CANVAS_REWARD("d4AClaimCanvasReward", "领取canvas的代币", false, SubscriberTypeEnum.EVENT, "d4AClaimCanvasRewardChainService"),
    D4APause("D4APause", "D4A停机", false, SubscriberTypeEnum.EVENT, "changeD4APauseChainService", "0x03756be0dad4f14bb8556920f9019893f8a60f49227cb3ab9e6eeb532b52035c"),
    D4ASetProjectPaused("D4ASetProjectPaused", "DAO停机", false, SubscriberTypeEnum.EVENT,
            "d4ASetProjectPausedChainService", "0x79ff52eaccf789ab71a7f002bbae3c831805f57849c4a0ee35a91697c6a99cd8"),
    D4ASetCanvasPaused("D4ASetCanvasPaused", "Canvas停机", false, SubscriberTypeEnum.EVENT,
            "d4ASetCanvasPausedChainService", "0xe8489f753c4d5cf95e5b9099aa22a9c414b8c06e8513d33710217e02c26c245a"),
    ExchangeERC20ToETH("D4AExchangeERC20ToETH", "换取ETH", false, SubscriberTypeEnum.EVENT,
            "d4AExchangeERC20ToETHChainService", "0x674751277df8115e140ad56fbe245e13972f3eb35226d0b833c4053b37e46aca"),
    ROLE_GRANTED("roleGranted", "授权", false, SubscriberTypeEnum.EVENT, "roleGrantedChainService", "0x2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d"),
    REVOK_EROLE("revokeRole", "取消授权", false, SubscriberTypeEnum.EVENT, "revokeRoleChainService", "0xf6391f5c32d9c69d2a47ea670b442974b53935d1edc7fd64eb21e047a839171b"),
    MinterBlacklisted("MinterBlacklisted", "进入minter黑名单", false, SubscriberTypeEnum.EVENT,
            "minterBlacklistedChainService", "0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08"),
    CanvasCreatorBlacklisted("CanvasCreatorBlacklisted", "进入canvascreator黑名单", false, SubscriberTypeEnum.EVENT,
            "canvasCreatorBlacklistedChainService", "0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66"),
    MinterUnBlacklisted("MinterUnBlacklisted", "消除minter黑名单", false, SubscriberTypeEnum.EVENT,
            "minterUnBlacklistedChainService", "0x4e078e1ac95a378c6196ca1055de7c80c48864a5be39d25b5b2e573e234137f9"),
    CanvasCreatorUnBlacklisted("CanvasCreatorUnBlacklisted", "消除canvascreator黑名单", false, SubscriberTypeEnum.EVENT,
            "canvasCreatorUnBlacklistedChainService", "0x50caa9f913dfbe6b84ea7839f03bcd87ff014cb9313d9633526e342e27c75404"),
    WhitelistModified("WhitelistModified", "白名单修改", false, SubscriberTypeEnum.EVENT, "whitelistModifiedChainService", "0x019cd1cc810dc2aacd4ef1720947a9f55bf58be7d83d6eeaf01330dd110531e8"),
    CREATE_PROJECT_FEE("createProjectFee", "铸造projet手续费", true, SubscriberTypeEnum.VALUE, "", ""),
    CREATE_CANVAS_FEE("createCanvasFee", "铸造canvas手续费", true, SubscriberTypeEnum.VALUE, "", ""),
    MintCapAdded("MintCapAdded", "添加铸造上限", false, SubscriberTypeEnum.EVENT, "mintCapAddedChainService", "0x21f6fa02312382a44060ea83b88540e2bacbf8142d73d968893a89adfdada979"),
    MintCapSet("MintCapSet", "修改黑白名单相关参数", false, SubscriberTypeEnum.EVENT, "mintCapSetChainService", "0x62285c51c0930afb5eecd599bbc38a6fbb7437a1a7c35693411877697d72b50f"),
    NewD4ARoyaltySplitter("NewD4ARoyaltySplitter", "创建splitter合约", false, SubscriberTypeEnum.EVENT,
            "newD4ARoyaltySplitterChainService", "0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e"),
    ETHTransfered("ETHTransfered", "二次交易版税收入", false, SubscriberTypeEnum.EVENT, "ethTransferedChainService", ""),
    DaoRatioSet("DaoRatioSet", "dao收益分配方式监听", false, SubscriberTypeEnum.EVENT, "daoRatioSetChainService", "0x6da23ff0efb72ad703b5db760470a8e0dbd704a3436413b3fa7544c08b177956"),
    // D4AClaimNftMinterReward("d4AClaimNftMinterReward", "minter领取代币收益", false, SubscriberTypeEnum.EVENT, "d4AClaimNftMinterRewardChainService"),
    // CreateProjectParamEmitted("createProjectParamEmitted", "minter领取代币收益", false, SubscriberTypeEnum.EVENT,"createProjectParamEmittedChainService","0x5293dcd92b85333f5fc9125fe13c673db6abdefe8803869ce1c7c802b1751ee7"),
    DaoPriceTemplateSet("DaoPriceTemplateSet", "设置 DAO 的变价规则", false, SubscriberTypeEnum.EVENT,
            "daoPriceTemplateSetChainService", "0xf0145078e0bd87d04ea9c14c072b1feba494bf6ed8c435e6a7eab084df8f49bb"),
    DaoNftMaxSupplySet("DaoNftMaxSupplySet", "设置 DAO NFT 发放总量", false, SubscriberTypeEnum.EVENT,
            "daoNftMaxSupplySetChainService", "0x05d033a31adeedf0d7e1b76da661d949ff1289f30eeeabbc94a9776dba5a70cc"),
    DaoMintableRoundSet("DaoMintableRoundSet", "设置 DAO 总铸造周期", false, SubscriberTypeEnum.EVENT,
            "daoMintableRoundSetChainService", "0x07c5b1187421ac218a48ed5320cb21023dcca064c3f92dbafb84de3fd0e816c3"),
    DaoFloorPriceSet("DaoFloorPriceSet", "设置 DAO 地板价", false, SubscriberTypeEnum.EVENT,
            "daoFloorPriceSetChainService", "0x0b852b58b96c796669f6ef71d8a84736cd92a2a96304e5cbb6c0e7be16d1af6f"),
    CanvasRebateRatioInBpsSet("CanvasRebateRatioInBpsSet", "设置 Canvas 下 Mint NFT 时 ETH 折扣比例", false, SubscriberTypeEnum.EVENT,
            "canvasRebateRatioInBpsSetChainService", ""),
    PERIOD_BLOCK("periodBlock", "每个drb的区块数", true, SubscriberTypeEnum.VALUE, "", ""),
    BasicDaoUnlocked("BasicDaoUnlocked", "basic DAO 解锁", false, SubscriberTypeEnum.EVENT, "basicDaoUnlockedChainService", "0xf5c31a48466f5e00cf0e8f0e944d7476a8c320f2751fd641ade2f58ef088acfb"),
    DaoTokenSupplySet("DaoTokenSupplySet", "追加DaoToken的发放额度", false, SubscriberTypeEnum.EVENT, "daoTokenSupplySetChainService", "0x1ce594a22cbf305011c4dd2f9477f4f1466e17c64268b751e685c4d917b1d7c2"),
    DailyMintCapSet("DailyMintCapSet", "每日铸造上限DailyMintCap", false, SubscriberTypeEnum.EVENT, "dailyMintCapSetChainService", "0x7a629aa9abc266608c18c2df5e79bf5f86dbb361f6978daaf3912c7bc787afca"),
    CreateContinuousProjectParamEmitted("CreateContinuousProjectParamEmitted", "protoDao创建扩展", false, SubscriberTypeEnum.EVENT, "createContinuousProjectParamEmittedChainService", "0x19f1ef28c13e9bb7b435e2967c9b3a6f9cfe800a04df8b512fb4e048ebfa0526"),
    DaoUnifiedPriceSet("DaoUnifiedPriceSet", "protoDao修改unifiedPrice", false, SubscriberTypeEnum.EVENT, "daoUnifiedPriceSetChainService", "0x0dd6824c8aef020ba0d0901c050ce28ebd7dcec24a6c5c7dc8b12416f498bec2"),
    // CreateProjectParamEmittedForFunding("CreateProjectParamEmittedForFunding", "CreateProjectParamEmittedForFunding", false, SubscriberTypeEnum.EVENT, "createProjectParamEmittedForFundingChainService","0x5293dcd92b85333f5fc9125fe13c673db6abdefe8803869ce1c7c802b1751ee7"),
    //    CreateContinuousProjectParamEmittedForFunding("CreateContinuousProjectParamEmittedForFunding", "CreateContinuousProjectParamEmittedForFunding", false, SubscriberTypeEnum.EVENT, "createContinuousProjectParamEmittedForFundingChainService"),
    ChildrenSet("ChildrenSet", "ChildrenSet", false, SubscriberTypeEnum.EVENT, "childrenSetChainService", "0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959"),
    NewProjectForFunding("NewProjectForFunding", "NewProjectForFunding", false, SubscriberTypeEnum.EVENT, "newProjectForFundingChainService", "0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693"),
    //  NewCanvasForFunding("NewCanvasForFunding", "NewCanvasForFunding", false, SubscriberTypeEnum.EVENT, "newCanvasForFundingChainService",""),
    NewCanvasForMint("NewCanvasForMint", "NewCanvasForMint", false, SubscriberTypeEnum.EVENT, "newCanvasForMintChainService", "0x85385e7db108825bb4c110598af68567989405e8919a91ca247eaa0b9f73249b"),
    DaoBlockRewardForSelf("DaoBlockRewardForSelf", "DaoBlockRewardForSelf", false, SubscriberTypeEnum.EVENT, "daoBlockRewardForSelfChainService", "0xccb9bf29ec765002b5719eee19b25c972b2c4c01215bb60b6a776bacb9566e5b"),
    DaoBlockRewardDistributedToRedeemPool("DaoBlockRewardDistributedToRedeemPool", "DaoBlockRewardDistributedToRedeemPool", false, SubscriberTypeEnum.EVENT, "daoBlockRewardDistributedToRedeemPoolChainService", "0x9f6b892d35d68ebecd76097d2b2eae7920f4e23205474ef71b5c720d4802754a"),
    DaoBlockRewardDistributedToChildrenDao("DaoBlockRewardDistributedToChildrenDao", "DaoBlockRewardDistributedToChildrenDao", false, SubscriberTypeEnum.EVENT, "daoBlockRewardDistributedToChildrenDaoChainService", "0x65a307c9af17c04367dfad6e6a3d80ddd5a551e4a01499572e32e71d544d0acc"),
    NewPoolsForFunding("NewPoolsForFunding", "NewPoolsForFunding", false, SubscriberTypeEnum.EVENT, "newPoolsForFundingChainService", "0x8d5ea2d27d4f1e4d950c9e1187b29f34bea520f079834249e9dc40141f85aa53"),
    RatioForFundingSet("RatioForFundingSet", "RatioForFundingSet", false, SubscriberTypeEnum.EVENT, "ratioForFundingSetChainService", "0xdad95cbf6cfc944fb83b420714234dcd2d6b616e9ef36ec544b7354a099261f3"),
    PDClaimDaoCreatorReward("PDClaimDaoCreatorReward", "PDClaimDaoCreatorReward", false, SubscriberTypeEnum.EVENT, "claimDaoCreatorRewardChainService", "0xeeb8b359b15aca07a16fbd5fb4846359bd6ed4bff6131953b5b5cb866f8d7b5b"),
    PDClaimCanvasReward("PDClaimCanvasReward", "PDClaimCanvasReward", false, SubscriberTypeEnum.EVENT, "claimCanvasRewardChainService", "0x7afcf29a9e75e05ec104211e3deaea3190f9765041996c8256faca31de27dff6"),
    PDClaimNftMinterReward("PDClaimNftMinterReward", "PDClaimNftMinterReward", false, SubscriberTypeEnum.EVENT, "claimNftMinterRewardChainService", "0x7b4b7f47bc41f6ab49e31a450f9ef9d2952f23772ce18d6b31db8323d7367b02"),
    //    PDClaimNftMinterRewardTopUp("PDClaimNftMinterRewardTopUp", "PDClaimNftMinterRewardTopUp", false, SubscriberTypeEnum.EVENT, "pDClaimNftMinterRewardTopUpChainService"),
    InitialTokenSupplyForSubDaoSet("InitialTokenSupplyForSubDaoSet", "InitialTokenSupplyForSubDaoSet", false, SubscriberTypeEnum.EVENT, "initialTokenSupplyForSubDaoSetChainService", "0xacb0e19b41c63a1947eae6c3fb7ae15be1f06c468dd10a6a4b3db811ca8df73b"),
    CreateProjectParamEmittedFour("CreateProjectParamEmittedFour", "CreateProjectParamEmittedFour", false, SubscriberTypeEnum.EVENT, "createProjectParamEmittedFourChainService", "0xde8640b504fda36af7e64a1eea3efa660f74707b37cbf53badd85ce7f739a49f"),
    DaoRemainingRoundSet("DaoRemainingRoundSet", "更改剩余的mintWindow", false, SubscriberTypeEnum.EVENT, "daoRemainingRoundSetChainService", "0xd04d592b8c6f2444cfc6e6987b4782e80404a49cf1fa31ae30dc9cf506169aed"),
    DaoRestart("DaoRestart", "重新开始dao", false, SubscriberTypeEnum.EVENT, "daoRestartChainService", "0x76f895b27d8be83c72fd1e214c24789c5890d50a8659e79ab0e2d881820bb1d3"),
    DaoInfiniteModeChanged("DaoInfiniteModeChanged", "修改无限模式", false, SubscriberTypeEnum.EVENT, "daoInfiniteModeChangedChainService", "0x73c929af5a25393fd383ddd048e069559a030e9a6c56bfb853e3872e16d3ac0a"),
    DaoBlockRewardTotal("DaoBlockRewardTotal", "当前drb第一次铸造获取token和eth总量", false, SubscriberTypeEnum.EVENT, "daoBlockRewardTotalChainService", "0xf7e786eb0e53f43421abdea5f824fb863f06e9d3da16186c870795a28853ea43"),
    getDaoCurrentRound("getDaoCurrentRound", "dao的当前window值", false, SubscriberTypeEnum.EVENT, "daoCurrentRoundChainService", ""),
    //    DaoTemplateSet("DaoTemplateSet", "设置价格变动和奖励发放的模板", false, SubscriberTypeEnum.EVENT,
//            "daoTemplateSetChainService");
    TopUpNftLock("TopUpNftLock", "获取work锁定信息", false, SubscriberTypeEnum.EVENT, "topUpNftLockChainService", "0xfa53e89c71ce0061bedbb1bcc28303df90f9a91aa1bf21f1167886beb715b250"),

    // 1.6新事件
    NewSemiOsGrantAssetPoolNft("NewSemiOsGrantAssetPoolNft", "国库给sub node打款", false, SubscriberTypeEnum.EVENT, "newSemiOsGrantAssetPoolNftChainService", "0x0509075f80cdea14ec28c1502071ee6f49da8ed96e40a22401f4d560b5411813"),
    NewSemiOsGrantTreasuryNft("NewSemiOsGrantTreasuryNft", "给国库打款信息", false, SubscriberTypeEnum.EVENT, "newSemiOsGrantTreasuryNftChainService", "0xf7ba94813f5fd7204e9bc67e024cc47eac2487950353607b3e3f512c83596de9"),

    // 下面的4个比例已经不抛
    // DefaultTopUpEthToRedeemPoolRatioSet("DefaultTopUpEthToRedeemPoolRatioSet", "修改默认eth-token的比例", false, SubscriberTypeEnum.EVENT, "defaultTopUpEthToRedeemPoolRatioSetChainService",""),
    // DefaultTopUpErc20ToTreasuryRatioSet("DefaultTopUpErc20ToTreasuryRatioSet", "修改默认token-eth的比例", false, SubscriberTypeEnum.EVENT, "defaultTopUpErc20ToTreasuryRatioSetChainService",""),
    // DaoTopUpEthToRedeemPoolRatioSet("DaoTopUpEthToRedeemPoolRatioSet", "修改某个dao下的eth-token的比例", false, SubscriberTypeEnum.EVENT, "daoTopUpEthToRedeemPoolRatioSetChainService",""),
    // DaoTopUpErc20ToTreasuryRatioSet("DaoTopUpErc20ToTreasuryRatioSet", "修改某个dao下的token-eth的比例", false, SubscriberTypeEnum.EVENT, "daoTopUpErc20ToTreasuryRatioSetChainService",""),
    // 截止需要修改

    // 新加事件--比例汇总
    TopUpEthSplitRatioSet("TopUpEthSplitRatioSet", "修改dao下的eth-token的比例汇总", false, SubscriberTypeEnum.EVENT, "topUpEthSplitRatioSetChainService", "0xc2d8f35cb6ec413664059e89c8d5dd8648d8384f9ad3f68932d6a61c4d7e528a"),
    TopUpErc20SplitRatioSet("TopUpErc20SplitRatioSet", "修改dao下的token-eth的比例汇总", false, SubscriberTypeEnum.EVENT, "topUpErc20SplitRatioSetChainService", "0xa2b89e73fb67bcd7a7b6a705962a90fbc23ae1d1bd55b39b6e9546f3f46d90cb"),

    NewSemiDaoErc721Address("NewSemiDaoErc721Address", "创建Dao时用于给sub dao打款赠送的erc721的地址", false, SubscriberTypeEnum.EVENT, "newSemiDaoErc721AddressChainService", "0xd3667753fa0651924c7260f11d6c97a9fed6546fd290520ec5564f244c5771c3"),

    NewSemiTreasury("NewSemiTreasury", "创建Main dao用于给国库打款赠送的erc721的地址", false, SubscriberTypeEnum.EVENT, "newSemiTreasuryChainService", "0x08eeb17ea135873525c910d41ad046f125008bc45892a06cb78708b1092336a8"),

    TopUpErc20Splitted("TopUpErc20Splitted", "用户将erc20转换为eth", false, SubscriberTypeEnum.EVENT, "topUpErc20SplittedChainService", "0x0538d6798823484e47b6060a0dc3fa8975021e3c3ff05879f2f7a3647c0d2c61"),

    // 添加plan计划监听
    NewSemiOsPlan("NewSemiOsPlan", "在seed nodes中创建plan激励计划", false, SubscriberTypeEnum.EVENT, "newSemiOsPlanChainService", "0x6aa2104d183ed7fffe1e940387d0c32625ecd6a56f93f815bac650cb373ef1af"),

    // 本地事件，更新plan周期
    // PLAN_CURRENT_ROUND("planCurrentRound", "plan当前的周期", false, SubscriberTypeEnum.VALUE, "",""),
    // 添加事件，更新plan周期
    getPlanCurrentRound("getPlanCurrentRound", "plan当前的周期", false, SubscriberTypeEnum.VALUE, "planCurrentRoundChainService", ""),    // 当前plan的周期
    // plan追加token监听
    PlanTotalRewardAdded("PlanTotalRewardAdded", "plan追加token", false, SubscriberTypeEnum.EVENT, "planTotalRewardAddedChainService", "0xbbb2633759ec4150dc2586285718c49f18853666eff18aad457c64ad8f196f7d"),
    // 用户Claimed奖励记录
    PlanRewardClaimSignal("PlanRewardClaimSignal", "Claimed奖励记录的信号事件", false, SubscriberTypeEnum.EVENT, "planRewardClaimSignalChainService", "0xfbe0524fbfedf9d86084e1fe0061e119a6decd7bbbeb73744bd1bd7318e6ad76"),
    // 此事件不做监听个
    PlanRewardClaimed("PlanRewardClaimed", "Claimed奖励记录", false, SubscriberTypeEnum.EVENT, "planRewardClaimedChainService", "0x6df0d30ccd4f96ceb372a7eee962371f89463b963d4970962dea4a6646185d09"),

    // 更新onChainBalance
    TopUpAccountUpdated("TopUpAccountUpdated", "更新onChainBalance", false, SubscriberTypeEnum.EVENT, "topUpAccountUpdatedChainService", "0xfc1f5941de2e389cc0deb38e38420424a2e876572dea8cfb2a297b39b022848a"),


    DaoEditInformationNftOwnerSet("DaoEditInformationNftOwnerSet", "转移Edit Information权限所有dao均有", false, SubscriberTypeEnum.EVENT, "daoEditInformationNftOwnerSetChainService", "0x2f42496343b0bf6886ffb703f087eea8f1a06b7802407bbb7f2974b05ff305fc"),
    DaoEditParameterNftOwnerSet("DaoEditParameterNftOwnerSet", "转移Edit On-chain parameters权限(所有dao均有)", false, SubscriberTypeEnum.EVENT, "daoEditParameterNftOwnerSetChainService", "0x70f896b18f07f19c3b8c713635192a57ac5d49729cd867aa5328d0be2cadea00"),
    DaoEditStrategyNftOwnerSet("DaoEditStrategyNftOwnerSet", "转移Edit Strategies权限(所有dao均有)", false, SubscriberTypeEnum.EVENT, "daoEditStrategyNftOwnerSetChainService", "0x75cac64a6b5dc64e5c863e66104f6ff3b7809cea055ba794027e25cdd7bbdf45"),
    DaoRewardNftOwnerSet("DaoRewardNftOwnerSet", "转移SubNodes Creator收益权限(所有dao均有)", false, SubscriberTypeEnum.EVENT, "daoRewardNftOwnerSetChainService", "0x789f647231d97f01880a8e96caf0763f01497db63b0e66f5a93536a45583a70c"),
    TreasurySetTopUpRatioOwnerSet("TreasurySetTopUpRatioOwnerSet", "转移设置top-up账户分流权限(仅main dao)", false, SubscriberTypeEnum.EVENT, "treasurySetTopUpRatioOwnerSetChainService", "0xfe13a4ee0df34165c526b06714fac432dc5ea4a6a8eb7c501538274616cc9c62"),
    TreasuryEditInformationOwnerSet("TreasuryEditInformationOwnerSet", "转移编辑seed nodes information的权限(仅main dao)", false, SubscriberTypeEnum.EVENT, "treasuryEditInformationOwnerSetChainService", "0xf56fbe351490b7aefbaf51475fd6c70f3a07567e5c2c24696ff9d6c0519577ca"),
    TreasuryTransferAssetOwnerSet("TreasuryTransferAssetOwnerSet", "转移国库的分配权限(仅main dao)", false, SubscriberTypeEnum.EVENT, "treasuryTransferAssetOwnerSetChainService", "0xfb266fbf14421b044f89efe9ad6f9bede353c6bec592a53c4fabba037fc4b9ea");

    @Getter
    private String type;

    @Getter
    private String name;

    /**
     * 仅启动时初始化的变量
     */
    @Getter
    private Boolean local;

    /**
     * 事件订阅还是值订阅
     */
    @Getter
    private SubscriberTypeEnum subscriberTypeEnum;

    @Getter
    private String tradeServiceName;

    @Getter
    private String topic;

    public static TradeTypeEnum queryByType(String type) {
        for (TradeTypeEnum tradeTypeEnum : TradeTypeEnum.values()) {
            if (tradeTypeEnum.getType().equals(type)) {
                return tradeTypeEnum;
            }
        }
        return null;
    }

    public static void setDefaultValue(TradeTypeEnum tradeTypeEnum, String value) {

        if (TradeTypeEnum.PROTOCOL_FEE_POOL.equals(tradeTypeEnum)) {
            value = CommonUtil.formatBytes32Address(value);// 转地址
        } else if (TradeTypeEnum.PERIOD_BLOCK.equals(tradeTypeEnum)) {
            //value不变
        } else {
            value = CommonUtil.hexToTenString(value);// 十六进制转十进制
        }
        if (tradeTypeEnum == null || StringUtils.isBlank(value)) {
            return;
        }
        // TODO 设置常量,但是该相关合约已经没有，需要重新定义逻辑..
        if (TradeTypeEnum.CURRENT_ROUND_LOCAL.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]CURRENT_ROUND_LOCAL:{}", value);
            ProtoDaoConstant.CURRENT_ROUND = value;
        } else if (TradeTypeEnum.MINT_D4A_FEE_RATIO.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]MINT_D4A_FEE_RATIO:{}", value);
            ProtoDaoConstant.MINT_D4A_FEE_RATIO = Integer.valueOf(value);
        } else if (TradeTypeEnum.RATIO_BASE.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]RATIO_BASE:{}", value);
            ProtoDaoConstant.RATIO_BASE = Integer.valueOf(value);
        } else if (TradeTypeEnum.MINT_PROJECT_FEE_RATIO.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]MINT_PROJECT_FEE_RATIO:{}", value);
            ProtoDaoConstant.MINT_PROJECT_FEE_RATIO = Integer.valueOf(value);
        } else if (TradeTypeEnum.PROTOCOL_FEE_POOL.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]PROTOCOL_FEE_POOL:{}", value);
            ProtoDaoConstant.protocol_fee_pool = CommonUtil.addHexPrefixIfNotExist(value);
        } else if (TradeTypeEnum.MINT_PROJECT_FEE_RATIO_FLAT_PRICE.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]MINT_PROJECT_FEE_RATIO_FLAT_PRICE:{}", value);
            ProtoDaoConstant.MINT_PROJECT_FEE_RATIO_FLAT_PRICE = Integer.valueOf(value);
        } else if (TradeTypeEnum.CREATE_PROJECT_FEE.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]CREATE_PROJECT_FEE:{}", value);
            ProtoDaoConstant.CREATE_PROJECT_FEE = Long.valueOf(value);
        } else if (TradeTypeEnum.CREATE_CANVAS_FEE.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]CREATE_CANVAS_FEE:{}", value);
            ProtoDaoConstant.CREATE_CANVAS_FEE = Long.valueOf(value);
        } else if (TradeTypeEnum.PERIOD_BLOCK.equals(tradeTypeEnum)) {
            log.info("[TradeTypeEnum]PERIOD_BLOCK data:{}", value);
            //0x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000075f9d200000000000000000000000000000000000000000000000d8d726b7177a80000
            String data = CommonUtil.removeHexPrefixIfExists(value);
            List<String> dataList = CommonUtil.splitBy32Bytes(data);
            String startDrb = CommonUtil.hexToTenString(dataList.get(0));
            String startBlock = CommonUtil.hexToTenString(dataList.get(1));
            String blocksPerDrbE18 = CommonUtil.hexToTenString(dataList.get(2));
            ProtoDaoConstant.PERIOD_BLOCK = new BigDecimal(blocksPerDrbE18).divide(new BigDecimal(ProtoDaoConstant.BASIC_RATIO)).longValue();
            log.info("[TradeTypeEnum]PERIOD_BLOCK:{}", ProtoDaoConstant.PERIOD_BLOCK);
        }
    }

    public static void main(String[] args) {
        String value = "0x000000000000000000000000f4267391072b27d76ed8f2a9655bcf5246013f2d";
        value = CommonUtil.formatBytes32Address(value);// 十六进制转十进制
        System.out.println(value);
    }

}
