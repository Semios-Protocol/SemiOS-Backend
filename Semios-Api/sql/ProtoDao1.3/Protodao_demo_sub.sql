-- 所有订阅的sql 39个
--D4ADrb 3
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x74d6aAca0fA8213F515A50995Aefbf47DA5b8A89','0x8a19c8bc',NULL,'http://172.31.31.55:9480/method/call',NULL,'2022-04-22 00:00:00','currentRound',0,0,1);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x74d6aAca0fA8213F515A50995Aefbf47DA5b8A89','0x8a19c8bc',NULL,NULL,NULL,'2022-04-22 00:00:00','currentRoundLocal',0,0,5);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x74d6aAca0fA8213F515A50995Aefbf47DA5b8A89','0xb8a242520000000000000000000000000000000000000000000000000000000000000000','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','periodBlock',0,0,5);

----1.3 去掉 PDProtocol 28
--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','newCanvas',0,0,3);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x5a2ad185679629c6f79a7ca8c3b2f337d1c8bce787a38af1b8a6b798a9d01e89','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AMintNFT',0,0,4);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x0759b1bb04364351c56f2db6d1a0283eb1a5d122b14dceec6df1240845150067','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AClaimProjectERC20Reward',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xbe12aa6db02b67b3415bcd1b86c2d53ad3b2a71feb9e0fd23f683b2e69f91840','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AClaimCanvasReward',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x513aa96d635def8ca996e250ecfb2839b372eae83235befdffb6eebe15cb9dcf','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4AExchangeERC20ToETH',0,0,4);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x03756be0dad4f14bb8556920f9019893f8a60f49227cb3ab9e6eeb532b52035c','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4APause',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x79ff52eaccf789ab71a7f002bbae3c831805f57849c4a0ee35a91697c6a99cd8','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4ASetProjectPaused',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xe8489f753c4d5cf95e5b9099aa22a9c414b8c06e8513d33710217e02c26c245a','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4ASetCanvasPaused',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','roleGranted',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xf6391f5c32d9c69d2a47ea670b442974b53935d1edc7fd64eb21e047a839171b','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','revokeRole',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x21f6fa02312382a44060ea83b88540e2bacbf8142d73d968893a89adfdada979','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','MintCapAdded',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x3cc55282','0x995046',NULL,NULL,'2022-04-22 00:00:00','createProjectFee',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x57385de3','0x995046',NULL,NULL,'2022-04-22 00:00:00','createCanvasFee',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x3ad38d8e','0x995046',NULL,NULL,'2022-04-22 00:00:00','protocol_fee_pool',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xbbbf7898','0x995046',NULL,NULL,'2022-04-22 00:00:00','mintD4aFeeRatio',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xc9dd758c','0x995046',NULL,NULL,'2022-04-22 00:00:00','mintPprojectFeeRatio',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xc2fa002e','0x995046',NULL,NULL,'2022-04-22 00:00:00','ratioBase',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x0c35f058','0x995046',NULL,NULL,'2022-04-22 00:00:00','mintProjectFeeRatioFlatPrice',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x6da23ff0efb72ad703b5db760470a8e0dbd704a3436413b3fa7544c08b177956','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoRatioSet',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x6a02a8bb04d2168b746d34c7ea23bf29355210733ad7113bd967be6585c1452f','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AClaimNftMinterReward',0,0,4);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xf5c31a48466f5e00cf0e8f0e944d7476a8c320f2751fd641ade2f58ef088acfb','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','BasicDaoUnlocked',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x1ce594a22cbf305011c4dd2f9477f4f1466e17c64268b751e685c4d917b1d7c2','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoTokenSupplySet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x05d033a31adeedf0d7e1b76da661d949ff1289f30eeeabbc94a9776dba5a70cc','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoNftMaxSupplySet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xf0145078e0bd87d04ea9c14c072b1feba494bf6ed8c435e6a7eab084df8f49bb','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoPriceTemplateSet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x7a629aa9abc266608c18c2df5e79bf5f86dbb361f6978daaf3912c7bc787afca','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DailyMintCapSet',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','MintCapSet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x07c5b1187421ac218a48ed5320cb21023dcca064c3f92dbafb84de3fd0e816c3','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoMintableRoundSet',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x0b852b58b96c796669f6ef71d8a84736cd92a2a96304e5cbb6c0e7be16d1af6f','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoFloorPriceSet',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x0dd6824c8aef020ba0d0901c050ce28ebd7dcec24a6c5c7dc8b12416f498bec2','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoUnifiedPriceSet',0,0,4);



--PermissionControl 5
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x717364d8749B0725884039E348C6136AF8Cde684','0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','MinterBlacklisted',0,0,107);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x717364d8749B0725884039E348C6136AF8Cde684','0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','CanvasCreatorBlacklisted',0,0,108);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x717364d8749B0725884039E348C6136AF8Cde684','0x4e078e1ac95a378c6196ca1055de7c80c48864a5be39d25b5b2e573e234137f9','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','MinterUnBlacklisted',0,0,109);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x717364d8749B0725884039E348C6136AF8Cde684','0x50caa9f913dfbe6b84ea7839f03bcd87ff014cb9313d9633526e342e27c75404','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','CanvasCreatorUnBlacklisted',0,0,110);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x717364d8749B0725884039E348C6136AF8Cde684','0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','WhitelistModified',0,0,111);

----D4ARoyaltySplitterFactory 1
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x764d00B3BF4a117A4374232E49dC3B3a336E9030','0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewD4ARoyaltySplitter',0,0,111);

--1.3 去掉 PDCreateProjectProxy 2
--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x0b24A4eF15087d5719d8722301aa23Bdfa3B382E','0x20e40b4b45d07bbe745da67054551581eccfc3bfe54147ac92cc2e38794d18e1','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','createProjectParamEmitted',0,0,9);

--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x0b24A4eF15087d5719d8722301aa23Bdfa3B382E','0x426be40f65ef9fd1545fd8b366b9a0b8c388973b5da308e190e19522a147aa96','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateContinuousProjectParamEmitted',0,0,110);





--1.3 追加的PDProtocol合约的订阅

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x5293dcd92b85333f5fc9125fe13c673db6abdefe8803869ce1c7c802b1751ee7','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateProjectParamEmittedForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x7fd82cbb3b22fb80a98fb77d267e675a34f4327b14854297193cc25ba6431845','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateContinuousProjectParamEmittedForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','ChildrenSet',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x49a295241ce7f599e4cc258fe07ca3da0d42e906d6d843c98132d98db1028687','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewProjectForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x73971ffe82974b27d11b38b1eebc413ecb7f7d0994c600988c57e2eaea58b19b','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewCanvasForFunding',0,0,10);



INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x65a307c9af17c04367dfad6e6a3d80ddd5a551e4a01499572e32e71d544d0acc','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardDistributedToChildrenDao',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x9f6b892d35d68ebecd76097d2b2eae7920f4e23205474ef71b5c720d4802754a','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardDistributedToRedeemPool',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xccb9bf29ec765002b5719eee19b25c972b2c4c01215bb60b6a776bacb9566e5b','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardForSelf',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x89ebd1d735f1c2ecaf93fdee1c2a5dfd774eb3c713288ee6f320049a385884e3','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewPoolsForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xfdcb1b296bde2329fba44dfcf5f7cbe81cad408551dbb5a66281ef5592202bc4','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','RatioForFundingSet',0,0,10);

-- PDClaimNftMinterRewardTopUp  这个不用监听了 通过查询进行查询了
--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x25d58b78180826ed58cba7237a513de7938b218272258a657ca76d534fd95050','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimNftMinterRewardTopUp',0,0,10);

--PDClaimDaoCreatorReward  PDClaimCanvasReward  PDClaimNftMinterReward

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x3dc08e4906aa208d8fdfa834e277401cb5225e3b1cbbba95d272339e83f0ebaa','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimDaoCreatorReward',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xce703b5c484c58e6dea0fa8768151a507705afbcd92d4b50b32093a0b0282b9d','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimCanvasReward',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xa3b1b3dab0d710d977b133f4de916f2218d407931630ea0238827632a2f131f6','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimNftMinterReward',0,0,10);

--InitialTokenSupplyForSubDaoSet 追加代币的事件
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0xacb0e19b41c63a1947eae6c3fb7ae15be1f06c468dd10a6a4b3db811ca8df73b','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','InitialTokenSupplyForSubDaoSet',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x269503c42d2277A22E43Eb52f6543Cd582e07964','0x85385e7db108825bb4c110598af68567989405e8919a91ca247eaa0b9f73249b','0x995046','http://172.31.31.55:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewCanvasForMint',0,0,10);
