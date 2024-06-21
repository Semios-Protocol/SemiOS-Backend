-- test
-- D4ADrb 3个
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x893705d0CcbE2f885eC0373035ba6A20C6acCab1','0x8a19c8bc',NULL,'http://172.31.20.253:9480/method/call',NULL,'2022-04-22 00:00:00','currentRound',0,0,1);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x893705d0CcbE2f885eC0373035ba6A20C6acCab1','0x8a19c8bc',NULL,NULL,NULL,'2022-04-22 00:00:00','currentRoundLocal',0,0,5);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x893705d0CcbE2f885eC0373035ba6A20C6acCab1','0xb8a242520000000000000000000000000000000000000000000000000000000000000000',NULL,'http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','periodBlock',0,0,5);

--PDProtocol 21个
--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x2fc14fb86219de0bf0ad396d862e5fdc60a3b9af127c5f6b81b0e82876a281fe','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','newProject',0,0,2);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','newCanvas',0,0,3);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x5a2ad185679629c6f79a7ca8c3b2f337d1c8bce787a38af1b8a6b798a9d01e89','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AMintNFT',0,0,4);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x0759b1bb04364351c56f2db6d1a0283eb1a5d122b14dceec6df1240845150067','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AClaimProjectERC20Reward',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xbe12aa6db02b67b3415bcd1b86c2d53ad3b2a71feb9e0fd23f683b2e69f91840','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AClaimCanvasReward',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x513aa96d635def8ca996e250ecfb2839b372eae83235befdffb6eebe15cb9dcf','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4AExchangeERC20ToETH',0,0,4);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x03756be0dad4f14bb8556920f9019893f8a60f49227cb3ab9e6eeb532b52035c','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4APause',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x79ff52eaccf789ab71a7f002bbae3c831805f57849c4a0ee35a91697c6a99cd8','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4ASetProjectPaused',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xe8489f753c4d5cf95e5b9099aa22a9c414b8c06e8513d33710217e02c26c245a','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','D4ASetCanvasPaused',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','roleGranted',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xf6391f5c32d9c69d2a47ea670b442974b53935d1edc7fd64eb21e047a839171b','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','revokeRole',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x21f6fa02312382a44060ea83b88540e2bacbf8142d73d968893a89adfdada979','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','MintCapAdded',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x3cc55282','0x75F970',NULL,NULL,'2022-04-22 00:00:00','createProjectFee',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x57385de3','0x75F970',NULL,NULL,'2022-04-22 00:00:00','createCanvasFee',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x3ad38d8e','0x75F970',NULL,NULL,'2022-04-22 00:00:00','protocol_fee_pool',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xbbbf7898','0x75F970',NULL,NULL,'2022-04-22 00:00:00','mintD4aFeeRatio',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xc9dd758c','0x75F970',NULL,NULL,'2022-04-22 00:00:00','mintPprojectFeeRatio',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xc2fa002e','0x75F970',NULL,NULL,'2022-04-22 00:00:00','ratioBase',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x0c35f058','0x75F970',NULL,NULL,'2022-04-22 00:00:00','mintProjectFeeRatioFlatPrice',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xd1cb51c66ef906ed6175d0cd55389ffcb998281ee95770e58eca5d2c7fc26d22','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoRatioSet',0,0,4);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x6a02a8bb04d2168b746d34c7ea23bf29355210733ad7113bd967be6585c1452f','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','d4AClaimNftMinterReward',0,0,4);




--PermissionControl 5个
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xB3c620d5D0C0DbC603E33fc5880eA4E49a6332a8','0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','MinterBlacklisted',0,0,107);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xB3c620d5D0C0DbC603E33fc5880eA4E49a6332a8','0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','CanvasCreatorBlacklisted',0,0,108);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xB3c620d5D0C0DbC603E33fc5880eA4E49a6332a8','0x4e078e1ac95a378c6196ca1055de7c80c48864a5be39d25b5b2e573e234137f9','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','MinterUnBlacklisted',0,0,109);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xB3c620d5D0C0DbC603E33fc5880eA4E49a6332a8','0x50caa9f913dfbe6b84ea7839f03bcd87ff014cb9313d9633526e342e27c75404','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','CanvasCreatorUnBlacklisted',0,0,110);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xB3c620d5D0C0DbC603E33fc5880eA4E49a6332a8','0x26dd922795109c9e6c1565d3065de7142bc2b2a4f252475e8606109fc380ab29','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','WhitelistModified',0,0,111);

----D4ARoyaltySplitterFactory
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xf01BBD34A4A1b740e8Ea2dfe7F655af3e336576D','0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewD4ARoyaltySplitter',0,0,111);



--PDCreateProjectProxy
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x0b24A4eF15087d5719d8722301aa23Bdfa3B382E','0x20e40b4b45d07bbe745da67054551581eccfc3bfe54147ac92cc2e38794d18e1','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','createProjectParamEmitted',0,0,9);

--BasicDaoUnlocked
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xf5c31a48466f5e00cf0e8f0e944d7476a8c320f2751fd641ade2f58ef088acfb','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','BasicDaoUnlocked',0,0,9);





DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.address = '0xf8a7DE8fa75B088367557287Dc166aFcafd41F09';

update subscribe set contract_address = '0x7913B7a2cd48440005148799f6fA4E72A3f48B4f',filter_id = null,status=0 where contract_address = '0xf8a7DE8fa75B088367557287Dc166aFcafd41F09';


DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.address = '0x6488ab72878ED2eDD25f04558f42a214f87ED8C9';

update subscribe set contract_address = '0xB3c620d5D0C0DbC603E33fc5880eA4E49a6332a8',filter_id = null,status=0 where contract_address = '0x6488ab72878ED2eDD25f04558f42a214f87ED8C9';




