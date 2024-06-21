alter table dao add column `exist_dao_id` varchar(200) DEFAULT NULL COMMENT '用户选择关联的旧 Basic DAO';
alter table dao add column `daily_mint_cap` int(11) DEFAULT '10000' COMMENT 'dao每天可以铸造的上限';

alter table dao add column `need_mintable_work` int(11) DEFAULT '1' COMMENT '是否需要创建 1000张 work 0-需要 1-不需要';
alter table dao add column `erc721_mint_cap` int(11) DEFAULT '0' COMMENT '是否开启erc721高优白名单 0-未开启 1-已开启' after mint_cap;

alter table work add column `social_links` text COMMENT '社交链接，多个根据逗号分隔';
alter table work add column `opensea_link` varchar(600) DEFAULT NULL COMMENT 'opensea链接地址';
alter table work add column `twitter_link` varchar(600) DEFAULT NULL COMMENT 'Twitter链接地址';
alter table work add column `discord_link` varchar(600) DEFAULT NULL COMMENT 'Discord链接地址';

select * from subscribe where trade_type = 'CreateContinuousProjectParamEmitted';

-- PermissionControl-proxy 这个都有监听 生产环境不用执行
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


--PDCreateProjectProxy  以下生产需要执行
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x0b24A4eF15087d5719d8722301aa23Bdfa3B382E','0x426be40f65ef9fd1545fd8b366b9a0b8c388973b5da308e190e19522a147aa96','0x75f9d2','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateContinuousProjectParamEmitted',0,0,110);


-- PDProtocol合约
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x1ce594a22cbf305011c4dd2f9477f4f1466e17c64268b751e685c4d917b1d7c2','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoTokenSupplySet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x05d033a31adeedf0d7e1b76da661d949ff1289f30eeeabbc94a9776dba5a70cc','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoNftMaxSupplySet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0xf0145078e0bd87d04ea9c14c072b1feba494bf6ed8c435e6a7eab084df8f49bb','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoPriceTemplateSet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x7a629aa9abc266608c18c2df5e79bf5f86dbb361f6978daaf3912c7bc787afca','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DailyMintCapSet',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','MintCapSet',0,0,9);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x07c5b1187421ac218a48ed5320cb21023dcca064c3f92dbafb84de3fd0e816c3','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoMintableRoundSet',0,0,9);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x0b852b58b96c796669f6ef71d8a84736cd92a2a96304e5cbb6c0e7be16d1af6f','0x752fcb','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoFloorPriceSet',0,0,9);


--生产环境需要执行
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0xd1cb51c66ef906ed6175d0cd55389ffcb998281ee95770e58eca5d2c7fc26d22%';

update subscribe set topics = '0x6da23ff0efb72ad703b5db760470a8e0dbd704a3436413b3fa7544c08b177956',filter_id = null,status=0 where topics = '0xd1cb51c66ef906ed6175d0cd55389ffcb998281ee95770e58eca5d2c7fc26d22';


--WhitelistModified
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x26dd922795109c9e6c1565d3065de7142bc2b2a4f252475e8606109fc380ab29%';

update subscribe set topics = '0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec',filter_id = null,status=0 where topics = '0x26dd922795109c9e6c1565d3065de7142bc2b2a4f252475e8606109fc380ab29';




