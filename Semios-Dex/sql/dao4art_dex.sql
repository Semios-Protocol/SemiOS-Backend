CREATE TABLE `liquidity_transaction` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `erc20_address` varchar(128) DEFAULT NULL COMMENT 'erc20地址',
  `erc20_name` varchar(50) DEFAULT NULL COMMENT 'erc20名称',
  `erc20_symbol` varchar(50) DEFAULT NULL COMMENT 'erc20Symbol',
  `trade_type` int NOT NULL COMMENT '交易类型1-swapErc20 2-swapEth 3-add 4-remove 5-burn',
  `transaction_hash` varchar(128) DEFAULT NULL COMMENT '交易链上hash',
  `block_time` bigint unsigned DEFAULT NULL COMMENT '上链时间',
  `in_token_amount` decimal(36,18) DEFAULT NULL COMMENT '交易时使用的代币',
  `out_token_amount` decimal(36,18) DEFAULT NULL COMMENT '交易时兑换出来的代币',
  `erc20_balance` decimal(36,18) DEFAULT NULL COMMENT 'erc20总余额',
  `eth_balance` decimal(36,18) DEFAULT NULL COMMENT 'eth总余额',
  `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '本次eth交易量',
  `pool_token_amount` decimal(36,18) DEFAULT NULL COMMENT '添加和减少流动性时的pool token数量',
  `user_address` varchar(128) DEFAULT NULL COMMENT '交易的用户地址',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `erc20_address_transaction_type` (`erc20_address`,`transaction_hash`,`trade_type`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='流通性交易表';


CREATE TABLE `erc20_liquidity` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `dao_id` int DEFAULT NULL COMMENT 'dao_id',
  `dao_status` int DEFAULT '0' COMMENT 'dao状态 0-未开始 1-已开始',
  `project_id` varchar(64) DEFAULT NULL COMMENT 'projectId',
  `erc20_address` varchar(128) DEFAULT NULL COMMENT 'erc20地址',
  `erc721_address` varchar(128) DEFAULT NULL COMMENT 'erc721地址',
  `erc20_name` varchar(50) DEFAULT NULL COMMENT 'erc20名称',
  `erc20_symbol` varchar(50) DEFAULT NULL COMMENT 'erc20 symbol',
  `erc20_block_time` bigint unsigned DEFAULT NULL COMMENT 'erc20上链时间',
  `eth_address` varchar(128) DEFAULT NULL COMMENT 'eth地址',
  `pair_address` varchar(128) DEFAULT NULL COMMENT '交易对地址',
  `pair_balance` decimal(36,18) DEFAULT NULL COMMENT 'pair总发行量',
  `erc20_order` int NOT NULL DEFAULT '0' COMMENT '创建交易对时erc20的顺序为第0个还是第1个',
  `erc20_balance` decimal(36,18) DEFAULT NULL COMMENT 'erc20总余额',
  `eth_balance` decimal(36,18) DEFAULT NULL COMMENT 'eth总余额',
  `transaction_hash` varchar(128) DEFAULT NULL COMMENT '交易对开通链上hash',
  `block_time` bigint unsigned DEFAULT NULL COMMENT '交易对开通上链时间',
  `last_swap_hash` varchar(300) DEFAULT NULL COMMENT '最近一次比例变更的hash',
  `create_address` varchar(128) DEFAULT NULL COMMENT '创建者用户地址',
  `is_del` int NOT NULL DEFAULT '0' COMMENT '是否删除 0-未删除 1-已删除',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `royalty_fee_modify_hash` varchar(128) DEFAULT NULL COMMENT 'ERC20的版税收益修改的交易hash',
  `royalty_fee` decimal(16,6) DEFAULT NULL COMMENT 'ERC20的版税收益',
  `dao_version` int DEFAULT '1' COMMENT 'dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本',
  `app_source` int DEFAULT '1' COMMENT 'app来源 1-dao4art 2-protodao',
  PRIMARY KEY (`id`),
  UNIQUE KEY `erc20_address` (`erc20_address`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='erc20流通性表';

CREATE TABLE `liquidity_price_record` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `type` int DEFAULT NULL COMMENT '类型 0-swap交易量 1-burn交易量',
  `erc20_address` varchar(128) DEFAULT NULL COMMENT 'erc20地址',
  `erc20_amount` decimal(36,18) DEFAULT NULL COMMENT '交易erc总20数量',
  `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '交易eth总数量',
  `trading_volume` decimal(36,18) DEFAULT NULL COMMENT '交易量',
  `price` decimal(36,18) DEFAULT NULL COMMENT '交易价格 都是token兑换eth价格',
  `record_time` bigint unsigned NOT NULL COMMENT '记录时间戳',
  `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `erc20_address_time` (`erc20_address`,`record_time`,`type`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='erc20流通性价格变更记录';


CREATE TABLE `liquidity_daily_statistics` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `erc20_address` varchar(128) DEFAULT NULL COMMENT 'erc20地址',
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
  PRIMARY KEY (`id`),
  UNIQUE KEY `erc20_address_time` (`erc20_address`,`record_time`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='erc20流通性每日零点统计数据';


CREATE TABLE `user_liquidity_statistics` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `erc20_address` varchar(128) DEFAULT NULL COMMENT 'erc20地址',
  `user_address` varchar(128) DEFAULT NULL COMMENT '用户地址',
  `user_id` varchar(128) DEFAULT NULL COMMENT '用户id',
  `user_logo` varchar(128) DEFAULT NULL COMMENT '用户logo',
  `is_contract` tinyint DEFAULT NULL COMMENT '是否为合约地址 0-否 1-是',
  `erc20_balance` decimal(36,18) DEFAULT NULL COMMENT 'erc20余额',
  `pool_token` decimal(36,18) DEFAULT NULL COMMENT 'pool token数量',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modify_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `sync_erc20_balance` tinyint DEFAULT '0' COMMENT '是否需要同步erc20Balance 0-否 1-是',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_erc20_address` (`erc20_address`,`user_address`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='用户代币拥有量';


CREATE TABLE `subscribe` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `contract_address` varchar(66) DEFAULT NULL COMMENT '合约地址',
  `topics` varchar(500) DEFAULT NULL COMMENT '订阅的主题或者方法名',
  `from_block` varchar(66) DEFAULT NULL COMMENT '开始块高度',
  `receive_address` varchar(66) DEFAULT NULL COMMENT '接收回调地址',
  `filter_id` varchar(200) DEFAULT NULL COMMENT '订阅ID',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `trade_type` varchar(45) DEFAULT NULL,
  `is_del` int DEFAULT '0',
  `status` int DEFAULT '0',
  `order_init` int DEFAULT NULL,
  `interval_time` int DEFAULT '10' COMMENT '间隔时间 秒 默认10秒',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='订阅记录';