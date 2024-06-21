alter table erc20_liquidity add column `erc721_address` VARCHAR(128) DEFAULT NULL COMMENT 'erc721地址' after erc20_address;


update dao set sync_dex = 1 where 1=1;

-------------------------test--------------------------------------

-- D4ADrb合约的periodBlock  每个drb多少个区块 -test TODO 地址不对
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x6199542a2a7594800d9b731f51bF02E8af130A7a','0xb8a242520000000000000000000000000000000000000000000000000000000000000000','0x752fcb','http://172.31.17.151:9280/transaction/call',NULL,'2022-04-22 00:00:00','periodBlock',0,0,2);

-------------------------demo--------------------------------------

-- D4ADrb合约的periodBlock  每个drb多少个区块 -demo
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x21B358F80B962B8aFa5C0EcB8E8e8877d9eA9633','0xb8a242520000000000000000000000000000000000000000000000000000000000000001','0x752fcb','http://172.31.17.151:9280/transaction/call',NULL,'2022-04-22 00:00:00','periodBlock',0,0,2);


-------------------------prod--------------------------------------

-- D4ADrb合约的periodBlock  每个drb多少个区块 -prod TODO 地址不对 确定传零或者1
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x893705d0CcbE2f885eC0373035ba6A20C6acCab1','0xb8a242520000000000000000000000000000000000000000000000000000000000000000','0x752fcb','http://127.0.0.1:9280/transaction/call',NULL,'2022-04-22 00:00:00','periodBlock',0,0,2);


DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics = '["0xb8a242520000000000000000000000000000000000000000000000000000000000000000"]';

update subscribe set topics = '0xb8a242520000000000000000000000000000000000000000000000000000000000000001',filter_id = null,status=0 where topics = '0xb8a242520000000000000000000000000000000000000000000000000000000000000000';