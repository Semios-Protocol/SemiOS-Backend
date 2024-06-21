alter table token_received_record add column `sync_dex` int DEFAULT NULL COMMENT '是否同步dex,只有swap需要同步 0-未同步 1-已同步';

alter table dao add column `sync_dex` int DEFAULT '0' COMMENT '是否需要同步dex0-不需要 1-需要';
alter table dao add column `dao_reward` decimal(16,6) DEFAULT NULL COMMENT 'DAO发放的所有ERC20数量';
alter table dao add column `burn_amount` decimal(16,6) DEFAULT NULL COMMENT 'DAOburn的erc20数量';
alter table dao add column  `liquidity_pool` int DEFAULT '0' COMMENT '是否开通流通性 0-未开通 1-开通';
alter table dao add column `erc20_name` varchar(45) DEFAULT NULL COMMENT 'project对应的erc20 name';
alter table dao add column `splitter_address` varchar(45) DEFAULT NULL COMMENT 'splitter合约地址';
alter table dao add column `royalty_fee_income` decimal(16,6) DEFAULT NULL COMMENT '版税二次交易收益';


update token_received_record set sync_dex = 0 where token_type = 1;
update token_received_record set sync_dex = 1 where token_type != 1;

Update dao set sync_dex = 1 where dao_status > 1;

update dao
set dao.dao_reward = (select dds.dao_reward from dao_drb_statistics dds where dds.dao_id = dao.id order by id desc limit 1)
where dao_reward is null;

update dao
set dao.burn_amount = (select sum(token_num) from token_received_record trr where trr.dao_number = dao.dao_number and trr.token_type = 1)
where dao.burn_amount is null;

CREATE TABLE `subscribe` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `contract_address` varchar(66) DEFAULT NULL COMMENT '合约地址',
  `topics` varchar(500) DEFAULT NULL COMMENT '订阅的主题或者方法名',
  `from_block` varchar(66) DEFAULT NULL COMMENT '开始块高度',
  `receive_address` varchar(66) DEFAULT NULL COMMENT '接收回调地址',
  `filter_id` varchar(200) DEFAULT NULL COMMENT '订阅ID',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `trade_type` varchar(45) DEFAULT NULL,
  `is_del` int(11) DEFAULT '0',
  `status` int(11) DEFAULT '0',
  `order_init` int(11) DEFAULT NULL,
  `interval_time` int(11) DEFAULT '10' COMMENT '间隔时间 秒 默认10秒',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 COMMENT='订阅记录';


CREATE TABLE `liquidity_transaction` (
    `id` INT NOT NULL AUTO_INCREMENT COMMENT 'id',
    `erc20_address` VARCHAR(128) DEFAULT NULL COMMENT 'erc20地址',
    `erc20_name` VARCHAR(50) DEFAULT NULL COMMENT 'erc20名称',
    `erc20_symbol` VARCHAR(50) DEFAULT NULL COMMENT 'erc20Symbol',
    `trade_type` INT(2) NOT NULL COMMENT '交易类型1-swapErc20 2-swapEth 3-add 4-remove 5-burn',
    `transaction_hash` VARCHAR(128) DEFAULT NULL COMMENT '交易链上hash',
    `block_time` bigint unsigned DEFAULT NULL COMMENT '上链时间',
    `in_token_amount` decimal(36,18) DEFAULT NULL COMMENT '交易时使用的代币',
    `out_token_amount` decimal(36,18) DEFAULT NULL COMMENT '交易时兑换出来的代币',
    `erc20_balance` decimal(36,18) DEFAULT NULL COMMENT 'erc20总余额',
    `eth_balance` decimal(36,18) DEFAULT NULL COMMENT 'eth总余额',
    `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '本次eth交易量',
    `pool_token_amount` decimal(36,18) DEFAULT NULL COMMENT '添加和减少流动性时的pool token数量',
    `user_address` VARCHAR(128) DEFAULT NULL COMMENT '交易的用户地址',
    `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY `erc20_address_transaction_type` (`erc20_address`,`transaction_hash`, `trade_type`)
)  ENGINE=INNODB COMMENT='流通性交易表';


CREATE TABLE `erc20_liquidity` (
    `id` INT NOT NULL AUTO_INCREMENT COMMENT 'id',
    `dao_id` INT DEFAULT NULL COMMENT 'dao_id',
    `dao_status` INT DEFAULT '0' COMMENT 'dao状态 0-未开始 1-已开始',
    `project_id` VARCHAR(64) DEFAULT NULL COMMENT 'projectId',
    `erc20_address` VARCHAR(128) DEFAULT NULL COMMENT 'erc20地址',
    `erc20_name` VARCHAR(50) DEFAULT NULL COMMENT 'erc20名称',
    `erc20_symbol` VARCHAR(50) DEFAULT NULL COMMENT 'erc20 symbol',
    `erc20_block_time` bigint unsigned DEFAULT NULL COMMENT 'erc20上链时间',
    `eth_address` VARCHAR(128) DEFAULT NULL COMMENT 'eth地址',
    `pair_address` VARCHAR(128) DEFAULT NULL COMMENT '交易对地址',
    `pair_balance` decimal(36,18) DEFAULT NULL COMMENT 'pair总发行量',
    `erc20_order` INT(2) NOT NULL DEFAULT '0' COMMENT '创建交易对时erc20的顺序为第0个还是第1个',
    `erc20_balance` decimal(36,18) DEFAULT NULL COMMENT 'erc20总余额',
    `eth_balance` decimal(36,18) DEFAULT NULL COMMENT 'eth总余额',
    `transaction_hash` VARCHAR(128) DEFAULT NULL COMMENT '交易对开通链上hash',
    `block_time` bigint unsigned DEFAULT NULL COMMENT '交易对开通上链时间',
    `last_swap_hash` VARCHAR(300) DEFAULT NULL COMMENT '最近一次比例变更的hash',
    `create_address` VARCHAR(128) DEFAULT NULL COMMENT '创建者用户地址',
    `is_del` INT(2) NOT NULL DEFAULT '0' COMMENT '是否删除 0-未删除 1-已删除',
    `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    `royalty_fee_modify_hash` VARCHAR(128) DEFAULT NULL COMMENT 'ERC20的版税收益修改的交易hash',
    `royalty_fee` decimal(16,6) DEFAULT NULL COMMENT 'ERC20的版税收益',
    PRIMARY KEY (id),
    UNIQUE KEY `erc20_address` (`erc20_address`)
)  ENGINE=INNODB COMMENT='erc20流通性表';

CREATE TABLE `liquidity_price_record` (
    `id` INT NOT NULL AUTO_INCREMENT COMMENT 'id',
    `type` INT DEFAULT NULL COMMENT '类型 0-swap交易量 1-burn交易量',
    `erc20_address` VARCHAR(128) DEFAULT NULL COMMENT 'erc20地址',
    `erc20_amount` decimal(36,18) DEFAULT NULL COMMENT '交易erc总20数量',
    `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '交易eth总数量',
    `trading_volume` decimal(36,18) DEFAULT NULL COMMENT '交易量',
    `price` decimal(36,18) DEFAULT NULL COMMENT '交易价格 都是token兑换eth价格',
    `record_time` bigint unsigned NOT NULL COMMENT '记录时间戳',
    `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY `erc20_address_time` (`erc20_address`,`record_time`,`type`)
)  ENGINE=INNODB  COMMENT='erc20流通性价格变更记录';


CREATE TABLE `liquidity_daily_statistics` (
    `id` INT NOT NULL AUTO_INCREMENT COMMENT 'id',
    `erc20_address` VARCHAR(128) DEFAULT NULL COMMENT 'erc20地址',
    `drb_number` int DEFAULT NULL COMMENT '当前drb的值',
    `erc20_amount` decimal(36,18) DEFAULT NULL COMMENT '交易erc总20数量',
    `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '交易eth总数量',
    `eth_swapped_rate` decimal(36,18) DEFAULT NULL COMMENT '兑换eth比例',
    `erc20_swapped_rate` decimal(36,18) DEFAULT NULL COMMENT '兑换erc20比例',
    `apy` decimal(36,18) DEFAULT NULL COMMENT 'APY',
    `burn_price` decimal(36,18) DEFAULT NULL COMMENT 'burn的价格',
    `asset_pool_total_amount` decimal(36,18) DEFAULT NULL COMMENT '资金池内总金额',
    `asset_pool_income` decimal(36,18) DEFAULT NULL COMMENT '资金池收入',
    `asset_pool_cost` decimal(36,18) DEFAULT NULL COMMENT '资金池支出',
    `asset_pool_variation` decimal(36,18) DEFAULT NULL COMMENT '资金池变化量',
    `record_time` bigint unsigned NOT NULL COMMENT '记录时间戳',
    `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (id),
    UNIQUE KEY `erc20_address_time` (`erc20_address`, `record_time`)
)  ENGINE=INNODB COMMENT='erc20流通性每日零点统计数据';


CREATE TABLE `user_liquidity_statistics` (
    `id` INT NOT NULL AUTO_INCREMENT COMMENT 'id',
    `erc20_address` VARCHAR(128) DEFAULT NULL COMMENT 'erc20地址',
    `user_address` VARCHAR(128) DEFAULT NULL COMMENT '用户地址',
    `user_id` VARCHAR(128) DEFAULT NULL COMMENT '用户id',
    `user_logo` VARCHAR(128) DEFAULT NULL COMMENT '用户logo',
    `is_contract` tinyint DEFAULT NULL COMMENT '是否为合约地址 0-否 1-是',
    `erc20_balance` decimal(36,18) DEFAULT NULL COMMENT 'erc20余额',
    `pool_token` decimal(36,18) DEFAULT NULL COMMENT 'pool token数量',
    `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    `sync_erc20_balance` tinyint DEFAULT '0' COMMENT '是否需要同步erc20Balance 0-否 1-是',
    PRIMARY KEY (id),
    UNIQUE KEY `user_erc20_address` (`erc20_address`,`user_address`)
)  ENGINE=INNODB  COMMENT='用户代币拥有量';
