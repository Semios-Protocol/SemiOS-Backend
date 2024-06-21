use protodao_fork;

truncate canvas;
truncate canvas_drb_statistics;
truncate dao;
truncate dao_drb_statistics;
truncate dao_strategy;
truncate favorites;
truncate shutdown_record;
truncate subscribe;
truncate token_received_record;
truncate `user`;
truncate user_harvest_token;
truncate white_list;
truncate `work`;

use subscribe_pro_fork;

truncate block_height;
truncate sub_num_value;
truncate subscriber;
truncate `transaction`;

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
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COMMENT='dao分配策略表';


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
  ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COMMENT='dao分配数量表';





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
  ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COMMENT='topup模式用户拥有eth和token的数量';


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
  ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COMMENT='dao代币追加记录';