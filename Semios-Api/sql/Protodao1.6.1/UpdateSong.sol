# 更新了创建dao的订阅流程,需要更新订阅表中的订阅事件

# 删除所有订阅服务表中的数据
delete from subscribe_pro.block_height where 1=1;
delete from subscribe_pro.sub_num_value where 1=1;
delete from subscribe_pro.subscriber where 1=1;
delete from subscribe_pro.transaction where 1=1;

# 删除创建dao主事件的其他订阅

# 正常情况下都会有
# emit CreateProjectParamEmitted
# cast sig-event "CreateProjectParamEmitted(bytes32 daoId,address daoFeePool,address token,address nft, (uint256,uint256,uint256,uint256,uint256,uint96,string,uint256),(bytes32,address[],bytes32,address[]),(address[],address[]),(uint32,(address,uint32)[]),(address,uint256)[],(uint8, uint256, uint8, uint256, bool),(bytes32,string,string),uint256,(uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256,uint256))"
delete from protodao.subscribe where topics = '0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b'; # protoDao主事件 CreateProjectParamEmitted

# 正常情况下都会有
# emit CreateContinuousProjectParamEmitted
# cast sig-event "CreateContinuousProjectParamEmitted(bytes32 existDaoId,bytes32 daoId,uint256 dailyMintCap,bool needMintableWork,bool unifiedPriceModeOff,uint256 unifiedPrice,uint256 reserveNftNumber,bool topUpMode,bool infiniteMode,bool erc20PaymentMode)"
delete from protodao.subscribe where topics = '0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7'; # protoDao创建扩展

# 正常情况下都会有
# emit NewProject
# cast sig-event "NewProject(bytes32 daoId, string daoUri, address token, address nft, uint256 royaltyFeeRatioInBps, bool isAncestorDao)"
delete from protodao.subscribe where topics = '0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693'; # 创建project

# 正常情况下会有
# emit NewCanvas
# cast sig-event "NewCanvas(bytes32 daoId, bytes32 canvasId, string canvasUri)"
delete from protodao.subscribe where topics = '0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686'; # 创建canvas

# 正常情况下会有
# emit NewPools
# cast sig-event "NewPools(bytes32 daoId,address daoAssetPool,address daoRedeemPool,uint256 daoTopUpEthToRedeemPoolRatio,uint256 daoTopUpErc20ToTreasuryRatio,bool isThirdPartyToken)"
delete from protodao.subscribe where topics = '0x8d5ea2d27d4f1e4d950c9e1187b29f34bea520f079834249e9dc40141f85aa53'; # 创建Pools

# 正常情况下都会有
# emit NewSemiDaoErc721Address
# cast sig-event "NewSemiDaoErc721Address(bytes32 daoId, address daoNft, address grantDaoNft)"
delete from protodao.subscribe where topics = '0xd3667753fa0651924c7260f11d6c97a9fed6546fd290520ec5564f244c5771c3'; # 创建Dao时用于给sub dao打款赠送的erc721的地址

# 如果是MainDao
# emit NewSemiTreasury
# cast sig-event "NewSemiTreasury(bytes32 daoId, address treasury, address grantTreasuryNft, uint256 initTokenSupply)"
delete from protodao.subscribe where topics = '0x08eeb17ea135873525c910d41ad046f125008bc45892a06cb78708b1092336a8'; # 创建Main dao 用于给国库打款赠送的erc721的地址

# 正常情况下都会有
# emit NewD4ARoyaltySplitter
# cast sig-event "NewD4ARoyaltySplitter(address)"
delete from protodao.subscribe where topics = '0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e'; # 创建splitter合约

# 正常情况下都会有
# emit ChildrenSet
# cast sig-event "ChildrenSet(bytes32 daoId,bytes32[] childrenDaoId,uint256[] erc20Ratios,uint256[] ethRatios,uint256 redeemPoolRatioETH,uint256 selfRewardRatioERC20,uint256 selfRewardRatioETH)"
delete from protodao.subscribe where topics = '0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959'; # TradeTypeEnum.ChildrenSet

# 正常情况下都会有
# emit WhitelistModified
# cast sig-event "WhitelistModified(bytes32 indexed daoId, (bytes32,address[],bytes32,address[]) )"
delete from protodao.subscribe where topics = '0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec'; # 白名单修改

# 如果有黑名单添加的话会有
# emit MinterBlacklisted
# cast sig-event "MinterBlacklisted(bytes32 indexed daoId, address indexed account)"
delete from protodao.subscribe where topics = '0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08'; # 进入minter黑名单

# 如果有黑名单添加的话会有
# emit CanvasCreatorBlacklisted
# cast sig-event "CanvasCreatorBlacklisted(bytes32 indexed daoId, address indexed account)"
delete from protodao.subscribe where topics = '0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66'; # 进入canvascreator黑名单

#cast sig-event "MintCapSet(bytes32 indexed daoId,uint32 daoMintCap,(address,uint32)[],(address,uint256)[])"
delete from protodao.subscribe where topics = '0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c'; # 修改黑白名单相关参数

delete from protodao.subscribe where topics = '0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d'; # TradeTypeEnum.RatioForFundingSet
delete from protodao.subscribe where topics = '0x2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d'; # 授权
delete from protodao.subscribe where topics = '0xf6391f5c32d9c69d2a47ea670b442974b53935d1edc7fd64eb21e047a839171b'; # 取消授权




#Add
# 下面的在订阅表中未订阅，不做处理
#1. 
# 正常情况下都会有
# emit NewD4AFeePool
# cast sig-event "NewD4AFeePool(address proxy, address admin)"
delete from protodao.subscribe where topics = '0x32ffa4a0c8de146dfe5248f35d6e2d44982c05be60deda44bad3244dcfc714e5'; 


#3. 
# 正常情况下会有
# emit DaoEditInformationNftOwnerSet
# cast sig-event "DaoEditInformationNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)"
delete from protodao.subscribe where topics = '0x2f42496343b0bf6886ffb703f087eea8f1a06b7802407bbb7f2974b05ff305fc'; 

#4. 
# 正常情况下会有
# emit DaoEditStrategyNftOwnerSet
# cast sig-event "DaoEditStrategyNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)"
delete from protodao.subscribe where topics = '0x75cac64a6b5dc64e5c863e66104f6ff3b7809cea055ba794027e25cdd7bbdf45'; 

#5. 
# 正常情况下会有
# emit DaoRewardNftOwnerSet
# cast sig-event "DaoRewardNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)"
delete from protodao.subscribe where topics = '0x789f647231d97f01880a8e96caf0763f01497db63b0e66f5a93536a45583a70c'; 

#6. 
# 如果 InheritTreeStorage.layout().inheritTreeInfos[daoId].isAncestorDao == True
# emit TreasuryEditInformationOwnerSet
# cast sig-event "TreasuryEditInformationOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)"
delete from protodao.subscribe where topics = '0xf56fbe351490b7aefbaf51475fd6c70f3a07567e5c2c24696ff9d6c0519577ca'; 

#6. 
# 如果 InheritTreeStorage.layout().inheritTreeInfos[daoId].isAncestorDao == True
# emit TreasuryTransferAssetOwnerSet
# cast sig-event "TreasuryTransferAssetOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)"
delete from protodao.subscribe where topics = '0xfb266fbf14421b044f89efe9ad6f9bede353c6bec592a53c4fabba037fc4b9ea'; 

#7. 
# 如果 InheritTreeStorage.layout().inheritTreeInfos[daoId].isAncestorDao == True
# emit TreasurySetTopUpRatioOwnerSet
# cast sig-event "TreasurySetTopUpRatioOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)"
delete from protodao.subscribe where topics = '0xfe13a4ee0df34165c526b06714fac432dc5ea4a6a8eb7c501538274616cc9c62'; 
