alter table dao add column `dao_redeem_pool` varchar(200) DEFAULT NULL COMMENT 'DAO Redeem池';
alter table dao add column `is_thirdparty_token`  int(11) DEFAULT '0' COMMENT '是否为外部ERC20 0-否 1-是';
--alter table dao add column `external_daotoken` varchar(50) DEFAULT NULL COMMENT '外部ERC20地址';
alter table dao add column `topup_mode`  int(11) DEFAULT '0' COMMENT '是否开启了TopUp模式 0-否 1-是';
alter table dao add column `is_ancestordao`  int(11) DEFAULT '0' COMMENT '是否为MainDAO 0-否 1-是';  --不用basicDao这个字段功能
alter table dao add column `eth_royalty_token` varchar(200) DEFAULT NULL COMMENT 'ETH分配策略';



alter table user_harvest_token add column `received_eth` decimal(36,18) DEFAULT '0' COMMENT '已领取eth数量';
alter table user_harvest_token add column `unclaimed_eth` decimal(36,18) DEFAULT '0' COMMENT '未领取eth数量';

alter table dao add column `received_eth` decimal(36,18) DEFAULT '0' COMMENT '已领取eth数量';
alter table dao add column `unclaimed_eth` decimal(36,18) DEFAULT '0' COMMENT '未领取eth数量';

alter table canvas add column `received_eth` decimal(36,18) DEFAULT '0' COMMENT '已领取eth数量';
alter table canvas add column `unclaimed_eth` decimal(36,18) DEFAULT '0' COMMENT '未领取eth数量';


alter table token_received_record add column `eth_num` decimal(36,18) DEFAULT '0' COMMENT '领取的eth数量';

alter table token_received_record add column `type` int(11) DEFAULT 0 COMMENT '0-dao token 1-eth';


alter table `work` add column `topup_mode`  int(11) DEFAULT '0' COMMENT '是否为topup模式铸造 0-非topup 1-topup下铸造topup的nft 2-topup下铸造非topup的nft';


--修改dex表中的数据
update erc20_liquidity set is_del = 1 where app_source = 2;





INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x5293dcd92b85333f5fc9125fe13c673db6abdefe8803869ce1c7c802b1751ee7','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateProjectParamEmittedForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x7fd82cbb3b22fb80a98fb77d267e675a34f4327b14854297193cc25ba6431845','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','CreateContinuousProjectParamEmittedForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','ChildrenSet',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x49a295241ce7f599e4cc258fe07ca3da0d42e906d6d843c98132d98db1028687','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewProjectForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x73971ffe82974b27d11b38b1eebc413ecb7f7d0994c600988c57e2eaea58b19b','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewCanvasForFunding',0,0,10);



INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x65a307c9af17c04367dfad6e6a3d80ddd5a551e4a01499572e32e71d544d0acc','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardDistributedToChildrenDao',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x9f6b892d35d68ebecd76097d2b2eae7920f4e23205474ef71b5c720d4802754a','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardDistributedToRedeemPool',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xccb9bf29ec765002b5719eee19b25c972b2c4c01215bb60b6a776bacb9566e5b','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoBlockRewardForSelf',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x89ebd1d735f1c2ecaf93fdee1c2a5dfd774eb3c713288ee6f320049a385884e3','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewPoolsForFunding',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xfdcb1b296bde2329fba44dfcf5f7cbe81cad408551dbb5a66281ef5592202bc4','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','RatioForFundingSet',0,0,10);

-- PDClaimNftMinterRewardTopUp  这个不用监听了 通过查询进行查询了
--INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
--VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x25d58b78180826ed58cba7237a513de7938b218272258a657ca76d534fd95050','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimNftMinterRewardTopUp',0,0,10);

--PDClaimDaoCreatorReward  PDClaimCanvasReward  PDClaimNftMinterReward

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x3dc08e4906aa208d8fdfa834e277401cb5225e3b1cbbba95d272339e83f0ebaa','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimDaoCreatorReward',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xce703b5c484c58e6dea0fa8768151a507705afbcd92d4b50b32093a0b0282b9d','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimCanvasReward',0,0,10);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xa3b1b3dab0d710d977b133f4de916f2218d407931630ea0238827632a2f131f6','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','PDClaimNftMinterReward',0,0,10);

--InitialTokenSupplyForSubDaoSet 追加代币的事件
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0xacb0e19b41c63a1947eae6c3fb7ae15be1f06c468dd10a6a4b3db811ca8df73b','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','InitialTokenSupplyForSubDaoSet',0,0,10);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x7041ea9c2d611014b4ea0401b1dee0676e971224','0x85385e7db108825bb4c110598af68567989405e8919a91ca247eaa0b9f73249b','0x995046','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewCanvasForMint',0,0,10);


CREATE TABLE `dao_allocation_strategy` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `origin_project_id` varchar(200) DEFAULT NULL COMMENT '分配的dao的原始projectid',
  `type` int(11) DEFAULT NULL COMMENT '0-dao token 1-eth',
  `dao_id` int(11) DEFAULT NULL COMMENT 'dao id',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
  `dao_name` varchar(200) DEFAULT NULL COMMENT 'DAO名称',
  `dao_number` int(11) DEFAULT NULL COMMENT 'dao编号',
  `royalty_proportion` decimal(16,6) DEFAULT NULL COMMENT '分配比例',
  `transaction_hash` varchar(200) DEFAULT NULL COMMENT '设置比例时交易hash',
  `block_time` varchar(45) DEFAULT NULL COMMENT '设置比例时交易hash上链时间',
  `royalty_type` int(11) DEFAULT '0' COMMENT '分配类型 0-非当前dao 1-redeem Asset Pool 2-selfReward 3-不出块比例',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='dao分配策略表';


CREATE TABLE `dao_allocation_amount` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `type` int(11) DEFAULT NULL COMMENT '0-daoToken分配 1-eth分配',
  `allocation_type` int(11) DEFAULT NULL COMMENT '0-其他dao分配 1-当前dao分配 2-当前dao的redeem池分配',
  `from_dao_id` varchar(200) DEFAULT NULL COMMENT '分配的fromDao的projectId',
  `to_dao_id` varchar(200) DEFAULT NULL COMMENT '分配的toDaoId的projectId',
  `token` varchar(200) DEFAULT NULL COMMENT 'token地址 0x0代表eth分配',
  `to_did` int(11) DEFAULT NULL COMMENT 'toDao的dao表的id',
  `to_dao_name` varchar(200) DEFAULT NULL COMMENT 'toDao的DAO名称',
  `to_dao_number` int(11) DEFAULT NULL COMMENT 'toDao的dao编号',
  `amount` decimal(36,18) DEFAULT NULL COMMENT '分配的数量',
  `round_drb` int(11) DEFAULT NULL COMMENT '分配时的drb',
  `transaction_hash` varchar(200) DEFAULT NULL COMMENT '交易hash',
  `block_time` varchar(45) DEFAULT NULL COMMENT '交易hash上链时间',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (`id`)
  ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='dao分配数量表';





CREATE TABLE `user_topup_harvest` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `user_address` varchar(200) DEFAULT NULL COMMENT '用户地址',
  `dao_id` int(11) DEFAULT NULL COMMENT 'mainDao dao id',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'mainDao projectId',
  `subdao_erc20_token` varchar(200) DEFAULT NULL COMMENT 'erc20Token地址',
  `erc20_amount` decimal(36,18) DEFAULT NULL COMMENT 'erc20余额',
  `eth_amount` decimal(36,18) DEFAULT NULL COMMENT 'eth余额',
  `round_drb` int(11) DEFAULT NULL COMMENT '分配时的drb',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (`id`)
  ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='topup模式用户拥有eth和token的数量';


CREATE TABLE `dao_append_token_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `dao_id` int(11) DEFAULT NULL COMMENT 'dao id',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao projectId',
  `origin_total_supply` decimal(36,18) DEFAULT NULL COMMENT '原始dao代币数量',
  `new_total_supply` decimal(36,18) DEFAULT NULL COMMENT '最新代币数量',
  `user_address` varchar(200) DEFAULT NULL COMMENT '追加人地址，非第三方时为空',
  `is_thirdparty` int(11) DEFAULT '0' COMMENT '是否为外部ERC20 0-否 1-是',
  `transaction_hash` varchar(200) DEFAULT NULL COMMENT '交易hash',
  `block_time` varchar(45) DEFAULT NULL COMMENT '交易hash上链时间',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (`id`),
    UNIQUE KEY `UNIQUE_transaction_hash` (`transaction_hash`,`project_id`)
  ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='dao代币追加记录';




