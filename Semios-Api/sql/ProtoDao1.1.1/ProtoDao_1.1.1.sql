alter table dao add column `global_dao_price` decimal(16,6) DEFAULT NULL COMMENT 'dao全局铸造价格';
alter table dao add column `generate_work_set` int(11) DEFAULT '0' COMMENT '设置自动生成work的总数量 0-为未设置';

update dao set global_dao_price = 0.01 where basic_dao = 2;

update dao set generate_work_set = 1000 where basic_dao = 2;

--update dao set need_mintable_work = 0;


--CreateContinuousProjectParamEmitted
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x426be40f65ef9fd1545fd8b366b9a0b8c388973b5da308e190e19522a147aa96%';

update subscribe set topics = '0x63294750143469ea04d7ea940d8379a96cb09041e146ef411ccaa3b8904b5272',filter_id = null,status=0 where topics = '0x426be40f65ef9fd1545fd8b366b9a0b8c388973b5da308e190e19522a147aa96';

--PDProtocol
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x0dd6824c8aef020ba0d0901c050ce28ebd7dcec24a6c5c7dc8b12416f498bec2','0x75F970','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoUnifiedPriceSet',0,0,4);


--mainnet-fork --PDProtocol
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x0dd6824c8aef020ba0d0901c050ce28ebd7dcec24a6c5c7dc8b12416f498bec2','0x75F970','http://172.31.16.128:9480/method/call',NULL,'2022-04-22 00:00:00','DaoUnifiedPriceSet',0,0,4);

--生产环境
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7913B7a2cd48440005148799f6fA4E72A3f48B4f','0x0dd6824c8aef020ba0d0901c050ce28ebd7dcec24a6c5c7dc8b12416f498bec2','0x75F970','http://172.31.20.253:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoUnifiedPriceSet',0,0,4);
