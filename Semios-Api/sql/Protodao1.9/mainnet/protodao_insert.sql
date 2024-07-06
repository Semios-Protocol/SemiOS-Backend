# insert subscribe data
# TODO 合约地址需要调整

# 0x60E771d7E4B7A8f8E7Fdc28d6E3852A4c556e546->0x63A647fca74D696457eB9a4378281756fbd7d071
# 0xF3A903602f808fB91EE7618cAf4b3dEa57CB9E29->0xB96Ab55CbA66834c92DD6d984F309a82a1c98b10
# 0x6199542a2a7594800d9b731f51bF02E8af130A7a->0x893705d0CcbE2f885eC0373035ba6A20C6acCab1

# TODO 回掉地址需要调整
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (1, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x1ce7903c69a8018cfe39951cc5e4afa327100389f4991ed7a6aa8f4083a6e33d', null, 'http://172.31.16.128:9480/transaction/call', null,  'd4AMintNFT', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (2, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x674751277df8115e140ad56fbe245e13972f3eb35226d0b833c4053b37e46aca', null, 'http://172.31.16.128:9480/transaction/call', null,  'D4AExchangeERC20ToETH', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (3, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x03756be0dad4f14bb8556920f9019893f8a60f49227cb3ab9e6eeb532b52035c', null, 'http://172.31.16.128:9480/transaction/call', null,  'D4APause', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (4, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x79ff52eaccf789ab71a7f002bbae3c831805f57849c4a0ee35a91697c6a99cd8', null, 'http://172.31.16.128:9480/transaction/call', null,  'D4ASetProjectPaused', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (5, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xe8489f753c4d5cf95e5b9099aa22a9c414b8c06e8513d33710217e02c26c245a', null, 'http://172.31.16.128:9480/transaction/call', null,  'D4ASetCanvasPaused', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (6, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x21f6fa02312382a44060ea83b88540e2bacbf8142d73d968893a89adfdada979', null, 'http://172.31.16.128:9480/transaction/call', null,  'MintCapAdded', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (7, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x3ad38d8e', null, 'http://172.31.16.128:9480/method/call', null,  'protocol_fee_pool', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (8, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xbbbf7898', null, 'http://172.31.16.128:9480/method/call', null,  'mintD4aFeeRatio', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (9, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xc2fa002e', null, 'http://172.31.16.128:9480/method/call', null,  'ratioBase', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (10, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x6da23ff0efb72ad703b5db760470a8e0dbd704a3436413b3fa7544c08b177956', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoRatioSet', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (11, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xf5c31a48466f5e00cf0e8f0e944d7476a8c320f2751fd641ade2f58ef088acfb', null, 'http://172.31.16.128:9480/transaction/call', null,  'BasicDaoUnlocked', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (12, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x1ce594a22cbf305011c4dd2f9477f4f1466e17c64268b751e685c4d917b1d7c2', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoTokenSupplySet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (13, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x05d033a31adeedf0d7e1b76da661d949ff1289f30eeeabbc94a9776dba5a70cc', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoNftMaxSupplySet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (14, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xf0145078e0bd87d04ea9c14c072b1feba494bf6ed8c435e6a7eab084df8f49bb', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoPriceTemplateSet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (15, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x7a629aa9abc266608c18c2df5e79bf5f86dbb361f6978daaf3912c7bc787afca', null, 'http://172.31.16.128:9480/transaction/call', null,  'DailyMintCapSet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (16, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x07c5b1187421ac218a48ed5320cb21023dcca064c3f92dbafb84de3fd0e816c3', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoMintableRoundSet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (17, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x0b852b58b96c796669f6ef71d8a84736cd92a2a96304e5cbb6c0e7be16d1af6f', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoFloorPriceSet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (18, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x0dd6824c8aef020ba0d0901c050ce28ebd7dcec24a6c5c7dc8b12416f498bec2', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoUnifiedPriceSet', 0, 1, 4, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (19, '0xB96Ab55CbA66834c92DD6d984F309a82a1c98b10', '0x4e078e1ac95a378c6196ca1055de7c80c48864a5be39d25b5b2e573e234137f9', null, 'http://172.31.16.128:9480/transaction/call', null,  'MinterUnBlacklisted', 0, 1, 109, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (20, '0xB96Ab55CbA66834c92DD6d984F309a82a1c98b10', '0x50caa9f913dfbe6b84ea7839f03bcd87ff014cb9313d9633526e342e27c75404', null, 'http://172.31.16.128:9480/transaction/call', null,  'CanvasCreatorUnBlacklisted', 0, 1, 110, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (21, '0x893705d0CcbE2f885eC0373035ba6A20C6acCab1', '0x8a19c8bc', null, 'http://172.31.16.128:9480/method/call', null,  'currentRound', 0, 1, 1, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (22, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x9f6b892d35d68ebecd76097d2b2eae7920f4e23205474ef71b5c720d4802754a', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoBlockRewardDistributedToRedeemPool', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (23, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xccb9bf29ec765002b5719eee19b25c972b2c4c01215bb60b6a776bacb9566e5b', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoBlockRewardForSelf', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (24, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x65a307c9af17c04367dfad6e6a3d80ddd5a551e4a01499572e32e71d544d0acc', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoBlockRewardDistributedToChildrenDao', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (25, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xeeb8b359b15aca07a16fbd5fb4846359bd6ed4bff6131953b5b5cb866f8d7b5b', null, 'http://172.31.16.128:9480/transaction/call', null,  'PDClaimDaoCreatorReward', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (26, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x7afcf29a9e75e05ec104211e3deaea3190f9765041996c8256faca31de27dff6', null, 'http://172.31.16.128:9480/transaction/call', null,  'PDClaimCanvasReward', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (27, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x7b4b7f47bc41f6ab49e31a450f9ef9d2952f23772ce18d6b31db8323d7367b02', null, 'http://172.31.16.128:9480/transaction/call', null,  'PDClaimNftMinterReward', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (28, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xacb0e19b41c63a1947eae6c3fb7ae15be1f06c468dd10a6a4b3db811ca8df73b', null, 'http://172.31.16.128:9480/transaction/call', null,  'InitialTokenSupplyForSubDaoSet', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (29, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xde8640b504fda36af7e64a1eea3efa660f74707b37cbf53badd85ce7f739a49f', null, 'http://172.31.16.128:9480/transaction/call', null,  'CreateProjectParamEmittedFour', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (30, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xd04d592b8c6f2444cfc6e6987b4782e80404a49cf1fa31ae30dc9cf506169aed', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoRemainingRoundSet', 0, 1, 110, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (31, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x73c929af5a25393fd383ddd048e069559a030e9a6c56bfb853e3872e16d3ac0a', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoInfiniteModeChanged', 0, 1, 110, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (32, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x76f895b27d8be83c72fd1e214c24789c5890d50a8659e79ab0e2d881820bb1d3', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoRestart', 0, 1, 110, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (33, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x85385e7db108825bb4c110598af68567989405e8919a91ca247eaa0b9f73249b', null, 'http://172.31.16.128:9480/transaction/call', null,  'NewCanvasForMint', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (34, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xf7e786eb0e53f43421abdea5f824fb863f06e9d3da16186c870795a28853ea43', null, 'http://172.31.16.128:9480/transaction/call', null,  'DaoBlockRewardTotal', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (35, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xfa53e89c71ce0061bedbb1bcc28303df90f9a91aa1bf21f1167886beb715b250', null, 'http://172.31.16.128:9480/transaction/call', null, 'TopUpNftLock', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (36, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x0509075f80cdea14ec28c1502071ee6f49da8ed96e40a22401f4d560b5411813', null, 'http://172.31.16.128:9480/transaction/call', null,  'NewSemiOsGrantAssetPoolNft', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (37, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xf7ba94813f5fd7204e9bc67e024cc47eac2487950353607b3e3f512c83596de9', null, 'http://172.31.16.128:9480/transaction/call', null,  'NewSemiOsGrantTreasuryNft', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (38, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xc2d8f35cb6ec413664059e89c8d5dd8648d8384f9ad3f68932d6a61c4d7e528a', null, 'http://172.31.16.128:9480/transaction/call', null,  'TopUpEthSplitRatioSet', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (39, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xa2b89e73fb67bcd7a7b6a705962a90fbc23ae1d1bd55b39b6e9546f3f46d90cb', null, 'http://172.31.16.128:9480/transaction/call', null,  'TopUpErc20SplitRatioSet', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (40, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x0538d6798823484e47b6060a0dc3fa8975021e3c3ff05879f2f7a3647c0d2c61', null, 'http://172.31.16.128:9480/transaction/call', null,  'TopUpErc20Splitted', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (41, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x62285c51c0930afb5eecd599bbc38a6fbb7437a1a7c35693411877697d72b50f', null, 'http://172.31.16.128:9480/transaction/call', null,  'MintCapSet', 0, 1, 9, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (42, '0xB96Ab55CbA66834c92DD6d984F309a82a1c98b10', '0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08', null, 'http://172.31.16.128:9480/transaction/call', null,  'MinterBlacklisted', 0, 1, 107, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (43, '0xB96Ab55CbA66834c92DD6d984F309a82a1c98b10', '0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66', null, 'http://172.31.16.128:9480/transaction/call', null,  'CanvasCreatorBlacklisted', 0, 1, 108, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (44, '0xB96Ab55CbA66834c92DD6d984F309a82a1c98b10', '0x019cd1cc810dc2aacd4ef1720947a9f55bf58be7d83d6eeaf01330dd110531e8', null, 'http://172.31.16.128:9480/transaction/call', null,  'WhitelistModified', 0, 1, 111, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (45, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959', null, 'http://172.31.16.128:9480/transaction/call', null,  'ChildrenSet', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (46, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xdad95cbf6cfc944fb83b420714234dcd2d6b616e9ef36ec544b7354a099261f3', null, 'http://172.31.16.128:9480/transaction/call', null,  'RatioForFundingSet', 0, 1, 10, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (47, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xbbb2633759ec4150dc2586285718c49f18853666eff18aad457c64ad8f196f7d', null, 'http://172.31.16.128:9480/transaction/call', null,  'PlanTotalRewardAdded', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (48, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xfc1f5941de2e389cc0deb38e38420424a2e876572dea8cfb2a297b39b022848a', null, 'http://172.31.16.128:9480/transaction/call', null,  'TopUpAccountUpdated', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (49, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0x6aa2104d183ed7fffe1e940387d0c32625ecd6a56f93f815bac650cb373ef1af', null, 'http://172.31.16.128:9480/transaction/call', null,  'NewSemiOsPlan', 0, 1, 3, 10);
INSERT INTO protodao_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (50, '0x63A647fca74D696457eB9a4378281756fbd7d071', '0xfbe0524fbfedf9d86084e1fe0061e119a6decd7bbbeb73744bd1bd7318e6ad76', null, 'http://172.31.16.128:9480/transaction/call', null,  'PlanRewardClaimSignal', 0, 1, 3, 10);
