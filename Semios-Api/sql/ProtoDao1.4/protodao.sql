-- protodao数据库
CREATE TABLE `dao_daily_statistics` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
    `dao_id` int(11) DEFAULT NULL COMMENT 'dao的id',
    `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
    `asset_pool_token_total_amount` decimal(36,18) DEFAULT NULL COMMENT '资金池内总金额',
    `asset_pool_token_income` decimal(36,18) DEFAULT NULL COMMENT '资金池收入',
    `asset_pool_token_cost` decimal(36,18) DEFAULT NULL COMMENT '资金池支出',
    `asset_pool_token_variation` decimal(36,18) DEFAULT NULL COMMENT '资金池变化量',
    `asset_pool_eth_total_amount` decimal(36,18) DEFAULT NULL COMMENT '资金池内总金额',
    `asset_pool_eth_income` decimal(36,18) DEFAULT NULL COMMENT '资金池收入',
    `asset_pool_eth_cost` decimal(36,18) DEFAULT NULL COMMENT '资金池支出',
    `asset_pool_eth_variation` decimal(36,18) DEFAULT NULL COMMENT '资金池变化量',
    `status` int(11) DEFAULT NULL COMMENT '计算状态 0-未计算 1-计算中 2-计算完成',
    `record_time` bigint unsigned DEFAULT NULL COMMENT '记录时间戳',
    `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY `project_id_record_time` (`project_id`, `record_time`)
)  ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Dao资金池每日零点统计数据';




INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xfd230129167daa1b6256b88e7de931428fa7b35c4fe5cb93cc7b28add4deb401','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateProjectParamEmittedFour',0,0,3);

alter table dao add column `infinite_mode` int(11) DEFAULT '0' COMMENT '是否开启无限模式，开启时返回1，关闭时返回0';
alter table dao add column `erc20_payment_mode` int(11) DEFAULT '0' COMMENT '是否开启Erc20支付模式，开启时为1，关闭时为0';
alter table dao add column `dao_start_block` varchar(60) DEFAULT NULL COMMENT 'dao开始的block区块高度';
alter table dao add column `duration` varchar(60) DEFAULT NULL COMMENT 'dao的每个mintableRound的持续时间';

alter table dao add column `remaining_mint_window` int(11) DEFAULT '0' COMMENT '剩余mintWindow';
alter table dao add column `current_round` int(11) DEFAULT '0' COMMENT 'dao当前周期数';
alter table dao add column `last_active_round` int(11) DEFAULT '0' COMMENT '最后一个活跃周期的编号';
alter table dao add column `erc20_token_decimals` int(11) DEFAULT NULL COMMENT 'Erc20支付模式下，decimals小数位数';

alter table dao_drb_statistics add column `record_time` bigint unsigned DEFAULT NULL COMMENT '归属时间';
alter table dao_drb_statistics add column `asset_pool_token_cost` decimal(36,18) DEFAULT NULL COMMENT '资金池支出';
alter table dao_drb_statistics add column `asset_pool_eth_cost` decimal(36,18) DEFAULT NULL COMMENT '资金池支出';

alter table subscribe add UNIQUE KEY `UNIQUE_filter_id` (`filter_Id`);

alter table dao_drb_statistics add column `contribution` decimal(36,18) DEFAULT NULL COMMENT '铸造nft的贡献度';

alter table dao_drb_statistics add column `erc20_amount` decimal(36,18) DEFAULT NULL COMMENT '当前drb mint后发放的所有erc20总量';
alter table dao_drb_statistics add column `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '当前drb mint后发放的所有eth总量';


--仅修改即可 看一下是否有这个trade_type = CreateContinuousProjectParamEmittedForFunding的记录，如果没有就插入下面这条，如果有就执行下面的修改。
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateContinuousProjectParamEmitted',0,0,110);

--这个需要在subscribe_pro库里执行
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7%';//这个topics是旧的topics
--这个需要在protodao库里执行
update subscribe set contract_address='0x7041ea9c2d611014b4ea0401b1dee0676e971224',topics = '0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7',filter_id = null,status=0 where topics = '0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7';
--contract_address 新的合约地址 topics新的topics地址， where topics = 旧的topics地址

--NewPoolsForFunding 改为NewPools  0x89ebd1d735f1c2ecaf93fdee1c2a5dfd774eb3c713288ee6f320049a385884e3 -> 0x88f566bf40d679b42ed2d0048a8bc81578fb0e246dac23b571e438044a5d5163
--这个需要在subscribe_pro库里执行
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x89ebd1d735f1c2ecaf93fdee1c2a5dfd774eb3c713288ee6f320049a385884e3%';
--这个需要在protodao库里执行
update subscribe set topics = '0x88f566bf40d679b42ed2d0048a8bc81578fb0e246dac23b571e438044a5d5163',filter_id = null,status=0 where topics = '0x89ebd1d735f1c2ecaf93fdee1c2a5dfd774eb3c713288ee6f320049a385884e3';

--RatioForFundingSet 改为RatioSet  0xfdcb1b296bde2329fba44dfcf5f7cbe81cad408551dbb5a66281ef5592202bc4 -> 0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0xfdcb1b296bde2329fba44dfcf5f7cbe81cad408551dbb5a66281ef5592202bc4%';
--这个需要在protodao库里执行
update subscribe set topics = '0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d',filter_id = null,status=0 where topics = '0xfdcb1b296bde2329fba44dfcf5f7cbe81cad408551dbb5a66281ef5592202bc4';


--NewProjectForFunding 改为 NewProject  0x49a295241ce7f599e4cc258fe07ca3da0d42e906d6d843c98132d98db1028687 -> 0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693
--这个需要在subscribe_pro库里执行
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x49a295241ce7f599e4cc258fe07ca3da0d42e906d6d843c98132d98db1028687%';
--这个需要在protodao库里执行
update subscribe set topics = '0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693',filter_id = null,status=0 where topics = '0x49a295241ce7f599e4cc258fe07ca3da0d42e906d6d843c98132d98db1028687';


--CreateContinuousProjectParamEmittedForFunding 改为 CreateContinuousProjectParamEmitted  0x7fd82cbb3b22fb80a98fb77d267e675a34f4327b14854297193cc25ba6431845 -> 0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7
--这个需要在subscribe_pro库里执行
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x7fd82cbb3b22fb80a98fb77d267e675a34f4327b14854297193cc25ba6431845%';
--这个需要在protodao库里执行
update subscribe set topics = '0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7',filter_id = null,status=0 where topics = '0x7fd82cbb3b22fb80a98fb77d267e675a34f4327b14854297193cc25ba6431845';


--NewCanvasForFunding 改为 NewCanvas  0x73971ffe82974b27d11b38b1eebc413ecb7f7d0994c600988c57e2eaea58b19b -> 0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686
--这个需要在subscribe_pro库里执行
DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics like '%0x73971ffe82974b27d11b38b1eebc413ecb7f7d0994c600988c57e2eaea58b19b%';
--这个需要在protodao库里执行
update subscribe set topics = '0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686',filter_id = null,status=0 where topics = '0x73971ffe82974b27d11b38b1eebc413ecb7f7d0994c600988c57e2eaea58b19b';

--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x7041Ea9C2d611014B4eA0401b1dee0676e971224','0x66ba869922a244a413f1d759e4bdd413b5fc7c72dc4a72012d6e119890248bfa09bcbdb798525e3dfa487e07968560a61f4427c56969be621282203ac16bf2f4','0x9bdfea','http://172.31.16.128:9480/method/call',NULL,'2022-04-22 00:00:00','getDaoCurrentRound',0,0,110);



--  DaoRemainingRoundSet    DaoInfiniteModeChanged  DaoRestart


--确保contract_address地址是demo环境的 receive_address是demo环境的
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041Ea9C2d611014B4eA0401b1dee0676e971224','0xd04d592b8c6f2444cfc6e6987b4782e80404a49cf1fa31ae30dc9cf506169aed','0x9bdfea','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoRemainingRoundSet',0,0,110);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041Ea9C2d611014B4eA0401b1dee0676e971224','0x73c929af5a25393fd383ddd048e069559a030e9a6c56bfb853e3872e16d3ac0a','0x9bdfea','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoInfiniteModeChanged',0,0,110);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041Ea9C2d611014B4eA0401b1dee0676e971224','0x76f895b27d8be83c72fd1e214c24789c5890d50a8659e79ab0e2d881820bb1d3','0x9bdfea','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoRestart',0,0,110);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x85385e7db108825bb4c110598af68567989405e8919a91ca247eaa0b9f73249b','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewCanvasForMint',0,0,10);






INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xf7e786eb0e53f43421abdea5f824fb863f06e9d3da16186c870795a28853ea43','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardTotal',0,0,10);




-- 第二版本的如下
-- 需要执行(1.4.2版本需求)
-- 把所有work的社交链接改为空
update work set social_links = '' where 1=1;


alter table dao add column `is_together_dao` int(11) DEFAULT '0' COMMENT '是否为聚合dao，0-否，1-是';
alter table dao add column `together_dao_id` int(11) DEFAULT NULL COMMENT '聚合dao的daoID';


alter table dao add column `token_holders` int(11) DEFAULT '0' COMMENT 'token的holders数量';

alter table dao add column `last_modify_round` int(11) DEFAULT '1' COMMENT '重新开始的currentRound数';

alter table dao add column `subdao_asset_pool_balance` varchar(200) DEFAULT NULL COMMENT '当前erc20余额';




