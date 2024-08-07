CREATE TABLE `canvas` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `canvas_name` varchar(200) DEFAULT NULL COMMENT 'canvas名称',
  `canvas_description` text COMMENT 'canvas描述信息',
  `canvas_logo` varchar(400) DEFAULT NULL COMMENT 'canvas的logo地址',
  `canvas_number` int(11) DEFAULT NULL COMMENT 'cavans的编号',
  `dao_id` int(11) DEFAULT NULL COMMENT 'dao的id',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
  `dao_number` int(11) DEFAULT NULL COMMENT '所属dao的编号',
  `owner_address` varchar(42) DEFAULT NULL COMMENT 'canvas创建者地址',
  `transaction_hash` varchar(66) DEFAULT NULL COMMENT 'canvas创建的交易hash',
  `opensea_link` varchar(400) DEFAULT NULL COMMENT 'opensea链接',
  `twitter_link` varchar(400) DEFAULT NULL COMMENT 'Twitter地址',
  `discord_link` varchar(400) DEFAULT NULL COMMENT 'discord地址',
  `block_time` varchar(45) DEFAULT NULL COMMENT 'Canvas创建的区块时间',
  `current_price` decimal(30,6) DEFAULT NULL COMMENT '当前铸造价格',
  `block_number` varchar(20) DEFAULT NULL COMMENT 'Canvas创建的区块号',
  `canvas_id` varchar(200) DEFAULT NULL COMMENT 'canvas创建时的canvasid',
  `drb_number` int(11) DEFAULT NULL COMMENT 'canvas创建时的drb号',
  `total_drb_number` int(11) DEFAULT '0' COMMENT 'canvas创建时dao剩余的区块数',
  `dao_floor_price` decimal(16,6) DEFAULT NULL COMMENT '所属dao的地板价',
  `canvas_uri` varchar(400) DEFAULT NULL COMMENT 'canvas的uri地址',
  `received_token` decimal(36,18) DEFAULT NULL COMMENT '已领取token数量',
  `unclaimed_token` decimal(36,18) DEFAULT NULL COMMENT '未领取token数量',
  `rest_drb` int(11) DEFAULT NULL,
  `favorite_amount` int(11) DEFAULT '0' COMMENT 'Canvas 收藏数',
  `dao_status` int(11) DEFAULT '2' COMMENT '2-已开始3-已结束',
  `canvas_status` int(11) DEFAULT '0' COMMENT '0-未创建1-已创建',
  `swap_token` decimal(36,18) DEFAULT NULL COMMENT '已兑换token数量',
  `swap_eth` decimal(36,18) DEFAULT NULL COMMENT '已兑换eht数量',
  `transfer_token` decimal(36,18) DEFAULT NULL COMMENT 'transfer token数量',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `social_links` text COMMENT '社交链接，多个根据逗号分隔',
  `dao_symbol` varchar(45) DEFAULT NULL COMMENT 'dao symbol',
  `royalty_token` decimal(16,6) DEFAULT NULL COMMENT 'royalty token 比例',
  PRIMARY KEY (`id`),
  UNIQUE KEY `dao_canvas_number` (`dao_number`,`canvas_number`)
) ENGINE=InnoDB AUTO_INCREMENT=82 DEFAULT CHARSET=utf8mb4 COMMENT='canvas画布';


CREATE TABLE `canvas_drb_statistics` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `dao_id` int(11) DEFAULT NULL COMMENT 'dao id',
  `canvas_id` int(11) DEFAULT NULL COMMENT 'Canvas id',
  `project_id` varchar(120) DEFAULT NULL COMMENT 'dao的project id',
  `drb_number` int(11) DEFAULT NULL COMMENT 'drb号',
  `mint_price` decimal(16,6) DEFAULT NULL COMMENT '当前铸造价格',
  `drb_vol` decimal(16,6) DEFAULT NULL COMMENT 'Canvas在当前区块铸造费用总和',
  `seven_day_drb_vol` decimal(16,6) DEFAULT NULL COMMENT 'Canvas在7days铸造费用总和',
  `total_vol` decimal(16,6) DEFAULT NULL COMMENT 'Canvas在本区块和之前所有区块铸造总费用',
  `ntvr` decimal(16,6) DEFAULT NULL COMMENT 'Canvas在本区块铸造总费用占DAO在本区块铸造总费用的占比',
  `dao_reward` decimal(16,6) DEFAULT NULL COMMENT 'Canvas在该区块结束后累计收到的ERC20数量',
  `dre` decimal(16,6) DEFAULT NULL COMMENT 'Mint Revenue+未来收益',
  `owners` int(11) DEFAULT NULL COMMENT 'Canvas在该区块结束时所有NFT所在地址的私钥拥有者去重计数',
  `nft` int(11) DEFAULT NULL COMMENT 'Canvas在该区块结束时所有NFT的数量',
  `work_amount` int(11) DEFAULT NULL COMMENT 'Canvas在该区块结束时所有work的数量',
  `seven_day_ntrv` decimal(16,6) DEFAULT NULL COMMENT 'Canvas本区块加上六个区块的铸造总费用占DAO在本区块加上六个区块铸造总费用的占比',
  `mint_revenue` decimal(16,6) DEFAULT NULL COMMENT 'Canvas的铸造总收入',
  `status` int(11) DEFAULT NULL COMMENT '计算状态 0-未计算 1-计算中 2-计算完成',
  `times` int(11) DEFAULT '0' COMMENT '计算失败次数',
  `mint_revenue_ex_tax` decimal(16,6) DEFAULT NULL COMMENT 'Canvas的铸造总收入*0.675',
  `drb_vol_ex_tax` decimal(16,6) DEFAULT NULL COMMENT 'Canvas在当前区块铸造费用总和*0.675',
  `dao_drb_vol_ex_tax` decimal(16,6) DEFAULT NULL COMMENT 'Dao的当前canvas在当前区块铸造费用总和*0.3',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `dao_drb_number` (`dao_id`,`canvas_id`,`drb_number`)
) ENGINE=InnoDB AUTO_INCREMENT=48 DEFAULT CHARSET=utf8mb4 COMMENT='canvas在drb的统计信息';



CREATE TABLE `dao` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `dao_name` varchar(200) DEFAULT NULL COMMENT 'DAO名称',
  `dao_manitesto` text COMMENT 'DAO宣言',
  `dao_description` text COMMENT 'DAO介绍',
  `dao_logo_url` varchar(300) DEFAULT NULL COMMENT 'DAOlogo地址',
  `dao_bg_banner` varchar(300) DEFAULT NULL COMMENT 'DAO背景图片地址',
  `dao_start_date` date DEFAULT NULL COMMENT 'DAO开始日期',
  `dao_start_drb` int(11) DEFAULT NULL COMMENT 'Dao start drb',
  `total_nft_casting` int(11) DEFAULT NULL COMMENT '最多可铸造的nft数量',
  `dao_mint_window` int(11) DEFAULT NULL COMMENT 'DAO周期',
  `dao_floor_price` decimal(16,6) DEFAULT NULL COMMENT 'DAO地板价',
  `dao_create_fee` float DEFAULT NULL COMMENT 'Dao创建手续费',
  `opensea_link` varchar(400) DEFAULT NULL COMMENT 'opensea链接地址',
  `twitter_link` varchar(400) DEFAULT NULL COMMENT 'Twitter链接地址',
  `discord_link` varchar(400) DEFAULT NULL COMMENT 'Discord链接地址',
  `dao_number` int(11) DEFAULT NULL COMMENT 'dao编号',
  `dao_status` int(11) DEFAULT NULL COMMENT 'Dao状态0-未创建1-已创建未开始2-已开始3-已结束 4-已停机',
  `block_time` varchar(45) DEFAULT NULL COMMENT '上链时间',
  `transaction_hash` varchar(66) DEFAULT NULL COMMENT 'dao创建的交易hash',
  `owner_address` varchar(42) DEFAULT NULL COMMENT 'Dao拥有者地址',
  `dao_uri` varchar(400) DEFAULT NULL COMMENT 'dao的uri地址',
  `drb_number` int(11) DEFAULT NULL COMMENT 'Dao创建时的drb区块',
  `block_number` varchar(45) DEFAULT NULL COMMENT 'Dao创建的链上区块号',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
  `received_token` decimal(36,18) DEFAULT NULL COMMENT '已领取token数量',
  `unclaimed_token` decimal(36,18) DEFAULT NULL COMMENT '未领取token数量',
  `fee_pool` varchar(200) DEFAULT NULL COMMENT 'project对应的asset pool地址',
  `erc20_token` varchar(200) DEFAULT NULL COMMENT 'project对应的erc20 token地址',
  `erc721_token` varchar(200) DEFAULT NULL COMMENT 'project对应的erc721 token地址',
  `royalty_fee` varchar(200) DEFAULT NULL COMMENT '二次售卖印花税设置',
  `erc20_total_supply` varchar(200) DEFAULT NULL COMMENT '发放erc20总量',
  `favorite_amount` int(11) DEFAULT '0' COMMENT 'Dao 收藏数',
  `canvas_floor_price` decimal(16,6) DEFAULT NULL COMMENT '当前dao下canvas最低价格',
  `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '当前dao兑换的eth数量',
  `swap_token` decimal(36,18) DEFAULT NULL COMMENT '已兑换token数量',
  `swap_eth` decimal(36,18) DEFAULT NULL COMMENT '已兑换eht数量',
  `dao_asset_pool` decimal(36,18) DEFAULT NULL COMMENT 'DAO在上一个区块结束时资金池里的总金额',
  `unchanged_token_amount` decimal(36,18) DEFAULT NULL COMMENT 'DAO在上一个区块结束时所有未兑换的代币数量',
  `transfer_token` decimal(36,18) DEFAULT NULL COMMENT 'transfer token数量',
  `canvas_created_whitelist` int(11) DEFAULT '0' COMMENT '是否开通创建canvas白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通',
  `canvas_created_blacklist` int(11) DEFAULT '0' COMMENT '是否开通创建canvas黑名单 0-未开通 1-开通',
  `minter_works_whitelist` int(11) DEFAULT '0' COMMENT '是否开通铸造nft白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通',
  `minter_works_blacklist` int(11) DEFAULT '0' COMMENT '是否开通铸造nft白名单 0-未开通 1-开通',
  `social_links` text COMMENT '社交链接，多个根据逗号分隔',
  `dao_symbol` varchar(45) DEFAULT NULL COMMENT 'dao symbol',
  `fresh_opensea` int(11) DEFAULT '0' COMMENT '是否需要刷新opensea 0-不需要 1-需要',
  `mint_cap` int(11) DEFAULT '0' COMMENT '是否开启高优白名单 0-未开启 1-已开启',
  `erc721_mint_cap` int(11) DEFAULT '0' COMMENT '是否开启erc721高优白名单 0-未开启 1-已开启',
  `global_mint_cap` int(11) DEFAULT '0' COMMENT 'dao全局铸造上限',
  `sync_dex` int(11) DEFAULT '0' COMMENT '是否需要同步dex0-不需要 1-需要',
  `dao_reward` decimal(36,18) DEFAULT NULL COMMENT 'DAO发放的所有ERC20数量',
  `burn_amount` decimal(36,18) DEFAULT NULL COMMENT 'DAOburn的erc20数量',
  `liquidity_pool` int(11) DEFAULT '0' COMMENT '是否开通流通性 0-未开通 1-开通',
  `erc20_name` varchar(45) DEFAULT NULL COMMENT 'project对应的erc20 name',
  `splitter_address` varchar(45) DEFAULT NULL COMMENT 'splitter合约地址',
  `royalty_fee_income` decimal(36,18) DEFAULT NULL COMMENT '版税二次交易收益',
  `dao_version` int(11) DEFAULT '1' COMMENT 'dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本',
  `royalty_token` varchar(200) DEFAULT NULL COMMENT '代币分配策略 结构体',
  `fixed_reserve_ratio` varchar(200) DEFAULT NULL COMMENT '一口价铸造收益分配策略 结构体',
  `unfixed_reserve_ratio` varchar(200) DEFAULT NULL COMMENT '非一口价铸造收益分配策略 结构体',
  `royalty_token_generation_method` int(11) DEFAULT '1' COMMENT '1-线性生成 2-衰减曲线',
  `royalty_token_lottery_mode` int(11) DEFAULT '0' COMMENT '0-关闭 1-开启 线性DRB释放ERC20',
  `canvas_price_fluctuation_method` int(11) DEFAULT '0' COMMENT '0-指数增加 1-线性增长',
  `fluctuation_method_factor` decimal(36,6) DEFAULT NULL COMMENT 'canvas价格增长系数',
  `royalty_token_generation_factory` decimal(16,6) DEFAULT NULL COMMENT '奖励发放衰减的系数',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `basic_dao` int(11) DEFAULT '1' COMMENT '是否为basic dao 1-proto dao 2- basic dao',
  `dao_flow` decimal(36,18) DEFAULT NULL COMMENT 'dao流水，超过2eth变为protodao',
  `add_work` int(11) DEFAULT '1' COMMENT 'protodao是否需要添加work 0-需要添加 1-不需要添加',
  `work_url_suffix` varchar(20) DEFAULT NULL COMMENT '生成work图片的后缀',
  `dao_work_url` varchar(200) DEFAULT NULL COMMENT '生成work图片的图片地址',
  `height` decimal(16,6) DEFAULT NULL COMMENT '图片在260宽度时的高度',
  `color` varchar(20) DEFAULT NULL COMMENT '生成work图片的背景色',
  `work_hash` varchar(50) DEFAULT NULL COMMENT '生成work图片的workHash',
  `exist_dao_id` varchar(200) DEFAULT NULL COMMENT '用户选择关联的旧 Basic DAO',
  `daily_mint_cap` int(11) DEFAULT '10000' COMMENT 'dao每天可以铸造的上限',
  `need_mintable_work` int(11) DEFAULT '1' COMMENT '是否需要创建 1000张 work 0-需要 1-不需要',
  PRIMARY KEY (`id`),
  UNIQUE KEY `dao_number` (`dao_number`)
) ENGINE=InnoDB AUTO_INCREMENT=81 DEFAULT CHARSET=utf8mb4 COMMENT='dao';



CREATE TABLE `dao_drb_statistics` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `dao_id` int(11) DEFAULT NULL COMMENT 'Dao id',
  `drb_number` int(11) DEFAULT NULL COMMENT 'Drb区块号',
  `floor_price` decimal(16,6) DEFAULT NULL COMMENT 'DAO在该区块结束时所有canvas的最低价',
  `seven_day_drb_vol` decimal(16,6) DEFAULT NULL COMMENT 'DAO在该区块和之前的六个区块（包括轮空的区块）铸造费用总和',
  `dao_asset_pool` decimal(16,6) DEFAULT NULL COMMENT 'DAO在该区块结束时资金池里的总金额',
  `dao_reward` decimal(16,6) DEFAULT NULL COMMENT 'DAO在该区块结束时所发放的所有ERC20数量',
  `canvas` int(11) DEFAULT NULL COMMENT 'DAO在该区块结束时canvas的数量',
  `owners` varchar(45) DEFAULT NULL COMMENT 'DAO在该区块结束时所有NFT所在地址的私钥拥有者去重计数',
  `nft` varchar(45) DEFAULT NULL COMMENT 'DAO在该区块结束时所有NFT的数量',
  `dre` varchar(45) DEFAULT NULL COMMENT 'Mint Revenue+未来收益',
  `drb_vol` decimal(16,6) DEFAULT NULL COMMENT '当前drb的铸造费用',
  `status` int(11) DEFAULT NULL COMMENT '计算状态 0-未计算 1-计算中 2-计算完成',
  `works` int(11) DEFAULT NULL COMMENT 'work的数量',
  `mint_revenue` decimal(16,6) DEFAULT NULL COMMENT 'dao的铸造总收入',
  `times` int(11) DEFAULT '0' COMMENT '计算失败次数',
  `mint_revenue_ex_tax` decimal(16,6) DEFAULT NULL COMMENT 'Dao的铸造总收入*0.3',
  `drb_vol_ex_tax` decimal(16,6) DEFAULT NULL COMMENT 'Dao在当前区块铸造费用总和*0.3',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `dao_drb_number` (`dao_id`,`drb_number`)
) ENGINE=InnoDB AUTO_INCREMENT=4064 DEFAULT CHARSET=utf8mb4 COMMENT='Dao在drb的统计信息';


CREATE TABLE `dao_strategy` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `type` int(11) DEFAULT NULL COMMENT '0-create_canvas 1-mint_work',
  `strategy_type` int(11) DEFAULT NULL COMMENT '1-白名单 2-erc721白名单 3-黑名单 4-高优白名单',
  `origin_address` longtext COMMENT 'erc721地址字符串数组-黑名单地址字符串数组-高优白名单地址DesignatedCap对象数组',
  `proof_id` int(11) DEFAULT NULL COMMENT '白名单时proof_store表id，erc721时代表daoId',
  `transaction_hash` varchar(200) DEFAULT NULL COMMENT '交易hash',
  `block_time` varchar(45) DEFAULT NULL COMMENT '交易hash上链时间',
  `dao_uri` varchar(400) DEFAULT NULL COMMENT 'dao的uri地址',
  `dao_id` int(11) DEFAULT NULL COMMENT 'dao的id',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
  `dao_number` int(11) DEFAULT NULL COMMENT 'dao的编号',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `is_valid` int(11) DEFAULT '1' COMMENT '0-无效 1-有效',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `strategy_type` (`type`,`strategy_type`,`dao_id`)
) ENGINE=InnoDB AUTO_INCREMENT=181 DEFAULT CHARSET=utf8mb4 COMMENT='dao黑白名单策略表';


CREATE TABLE `favorites` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `user_address` varchar(42) DEFAULT NULL COMMENT '用户地址',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `type` int(11) DEFAULT NULL COMMENT '收藏类型 0-DAO 1-Canvas 3-works',
  `favorite_id` varchar(66) DEFAULT NULL COMMENT '目标id',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_address` (`user_address`,`type`,`favorite_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='收藏';


CREATE TABLE `shutdown_record` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `type` int(11) NOT NULL COMMENT '0-平台停机 1-dao停机 2-canvas停机',
  `is_paused` int(11) DEFAULT '0' COMMENT '是否停机 0-停机 1-启动',
  `record_id` int(11) DEFAULT NULL COMMENT 'daoid或者canvasid',
  `block_number` varchar(45) DEFAULT NULL COMMENT '区块号',
  `block_time` varchar(45) DEFAULT NULL COMMENT '领取时间',
  `transaction_hash` varchar(120) DEFAULT NULL COMMENT '交易hash',
  `drb_number` int(11) DEFAULT NULL COMMENT '属于某个drb',
  `is_del` int(11) DEFAULT '0',
  `is_valid` int(11) NOT NULL DEFAULT '1' COMMENT '0-无效 1-有效',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `shutdown_id` varchar(200) DEFAULT NULL COMMENT 'projetId或者canvasId',
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQUE_transaction_hash` (`transaction_hash`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='停机记录表';



CREATE TABLE `subscribe` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `contract_address` varchar(66) DEFAULT NULL COMMENT '合约地址',
  `topics` varchar(500) DEFAULT NULL COMMENT '订阅的主题或者方法名',
  `from_block` varchar(66) DEFAULT NULL COMMENT '开始块高度',
  `receive_address` varchar(66) DEFAULT NULL COMMENT '接收回调地址',
  `filter_id` varchar(2000) DEFAULT NULL COMMENT '订阅ID',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `trade_type` varchar(45) DEFAULT NULL,
  `is_del` int(11) DEFAULT '0',
  `status` int(11) DEFAULT '0',
  `order_init` int(11) DEFAULT NULL,
  `interval_time` int(11) DEFAULT '10' COMMENT '间隔时间 秒 默认10秒',
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQUE_contract_address_topics` (`contract_address`,`topics`,`trade_type`)
) ENGINE=InnoDB AUTO_INCREMENT=267 DEFAULT CHARSET=utf8mb4 COMMENT='订阅记录';



CREATE TABLE `token_received_record` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `token_num` decimal(36,18) DEFAULT NULL COMMENT '领取token数量',
  `receive_type` int(11) DEFAULT NULL COMMENT '领取类型1-dao领取 2-canvas领取 3-transfer 4-mint领取',
  `receive_id` int(11) DEFAULT NULL COMMENT 'daoid或者canvasid',
  `receive_address` varchar(66) DEFAULT NULL COMMENT '领取人地址',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
  `canvas_id` varchar(200) DEFAULT NULL COMMENT 'canvas的id',
  `block_number` varchar(45) DEFAULT NULL COMMENT '区块号',
  `block_time` varchar(45) DEFAULT NULL COMMENT '领取时间',
  `transaction_hash` varchar(120) DEFAULT NULL COMMENT '交易hash',
  `drb_number` int(11) DEFAULT NULL COMMENT '属于某个drb',
  `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '当前dao兑换的eth数量',
  `token_type` int(11) DEFAULT '0' COMMENT '交易类型0-collect 1-swap 2-transfer',
  `from_address` varchar(200) DEFAULT NULL COMMENT 'from的地址',
  `to_address` varchar(200) DEFAULT NULL COMMENT 'to的地址',
  `dao_number` int(11) DEFAULT NULL COMMENT 'dao的编号',
  `token_num_balance` decimal(36,18) DEFAULT '0.000000000000000000' COMMENT 'transfer的token的剩余数量',
  `sync_dex` int(11) DEFAULT NULL COMMENT '是否同步dex,只有swap需要同步 0-未同步 1-已同步',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQUE_transaction_hash` (`transaction_hash`,`token_type`,`receive_type`,`receive_id`,`token_num`,`receive_address`)
) ENGINE=InnoDB AUTO_INCREMENT=49 DEFAULT CHARSET=utf8mb4 COMMENT='代币领取记录表';



CREATE TABLE `user` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `user_address` varchar(42) DEFAULT NULL COMMENT '用户地址',
  `user_name` varchar(500) DEFAULT NULL COMMENT '用户昵称',
  `user_introduction` text COMMENT '用户个人介绍',
  `avatar_address` varchar(400) DEFAULT NULL COMMENT '头像地址',
  `opensea_link` varchar(400) DEFAULT NULL COMMENT 'opensea链接',
  `twitter_link` varchar(400) DEFAULT NULL COMMENT 'Twitter链接',
  `discord_link` varchar(400) DEFAULT NULL COMMENT 'discord链接',
  `first_login_time` timestamp NULL DEFAULT NULL COMMENT '首次登陆时间',
  `sign_privacy_agreement` int(11) DEFAULT NULL COMMENT '是否签署隐私协议0-未签署1-已签署',
  `role` tinyint(4) DEFAULT '0' COMMENT '用户权限0-无权限 1-OPERATION_ROLE 2-DEFAULT_ADMIN_ROLE 3-DAO_ROLE 4-PROJECT_ROLE',
  `transaction_hash` varchar(66) DEFAULT NULL COMMENT '授权交易hash',
  `is_contract` tinyint(4) DEFAULT '0' COMMENT '是否为合约地址 0-否 1-是',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQUE_user_address` (`user_address`)
) ENGINE=InnoDB AUTO_INCREMENT=33 DEFAULT CHARSET=utf8mb4 COMMENT='用户信息表';


CREATE TABLE `user_harvest_token` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `dao_id` int(11) NOT NULL COMMENT 'dao id',
  `canvas_id` int(11) NOT NULL COMMENT 'canvas id',
  `total_token` decimal(36,18) DEFAULT NULL COMMENT '获得token的总数量',
  `received_token` decimal(36,18) DEFAULT NULL COMMENT '已领取未使用的token数量',
  `unclaimed_token` decimal(36,18) DEFAULT NULL COMMENT '未领取token数量',
  `transfer_token` decimal(36,18) DEFAULT NULL COMMENT 'transfer token数量',
  `swap_token` decimal(36,18) DEFAULT NULL COMMENT '已兑换token数量',
  `swap_eth` decimal(36,18) DEFAULT NULL COMMENT '已兑换eth数量',
  `user_address` varchar(128) DEFAULT NULL COMMENT '用户地址',
  `last_transaction_hash` varchar(66) DEFAULT NULL COMMENT '最后一次修改的hash',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `modify_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_dao` (`user_address`,`dao_id`)
) ENGINE=InnoDB AUTO_INCREMENT=31 DEFAULT CHARSET=latin1 COMMENT='用户mint获得的代币';


CREATE TABLE `white_list` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `user_address` varchar(42) DEFAULT NULL COMMENT '用户地址',
  `origin_address` longtext COMMENT '原始地址',
  `proof` longtext COMMENT 'proof信息',
  `proof_root_hash` varchar(200) DEFAULT NULL COMMENT 'proof根hash',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `is_valid` int(11) DEFAULT '1' COMMENT '0-无效 1-有效',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_address` (`user_address`,`proof_root_hash`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8mb4 COMMENT='黑白名单地址表';

CREATE TABLE `work` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `work_number` int(11) DEFAULT NULL COMMENT 'work编号',
  `image_url` varchar(400) DEFAULT NULL COMMENT '图片地址',
  `work_description` text COMMENT 'work描述',
  `canvas_id` varchar(200) DEFAULT NULL COMMENT '所属canvasid',
  `canvas_number` int(11) DEFAULT NULL COMMENT 'canvas编号',
  `dao_id` int(11) DEFAULT NULL COMMENT 'dao的id',
  `project_id` varchar(200) DEFAULT NULL COMMENT 'dao的projectid',
  `dao_number` int(11) DEFAULT NULL COMMENT 'dao的编号',
  `transaction_hash` varchar(66) DEFAULT NULL COMMENT 'Work铸造时的交易hash，默认为空',
  `creator_address` varchar(42) DEFAULT NULL COMMENT '创建人地址-canvas得拥有者地址',
  `minted_address` varchar(42) DEFAULT NULL COMMENT '铸造者地址',
  `minted_price` decimal(30,6) DEFAULT NULL COMMENT '铸造价格',
  `block_time` varchar(45) DEFAULT NULL COMMENT '铸造的区块时间',
  `drb_number` int(11) DEFAULT NULL COMMENT '铸造时所属drb区块',
  `owner_address` varchar(66) DEFAULT NULL COMMENT '当前owner地址',
  `work_uri` varchar(400) DEFAULT NULL COMMENT 'work的uri',
  `work_status` int(11) DEFAULT NULL COMMENT 'Work状态0-已创建1-已铸造2-已失效',
  `work_hash` varchar(66) DEFAULT NULL COMMENT 'work的hash值',
  `block_number` varchar(45) DEFAULT NULL COMMENT 'Work铸造时的区块号',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `favorite_amount` int(11) DEFAULT NULL COMMENT '收藏数',
  `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
  `bg_color` varchar(45) DEFAULT NULL COMMENT '图片背景色编码',
  `height` decimal(16,6) DEFAULT NULL COMMENT '图片在260宽度时的高度',
  `can_id` int(11) DEFAULT NULL COMMENT 'canvas表的id',
  `create_sign_hash` varchar(200) DEFAULT NULL COMMENT '创建时的签名hash，铸造的时候需要用',
  `price_type` int(11) DEFAULT '0' COMMENT '0-canvas_price 1-fixed_price',
  `fixed_price` decimal(30,6) DEFAULT NULL COMMENT '一口价',
  `generate` int(11) DEFAULT '2' COMMENT '是否自动生成 1-自动生成 2-上传的',
  `social_links` text COMMENT '社交链接，多个根据逗号分隔',
  `opensea_link` varchar(400) DEFAULT NULL COMMENT 'opensea链接地址',
  `twitter_link` varchar(400) DEFAULT NULL COMMENT 'Twitter链接地址',
  `discord_link` varchar(400) DEFAULT NULL COMMENT 'Discord链接地址',
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQUE_transaction_hash` (`transaction_hash`,`dao_id`,`work_number`)
) ENGINE=InnoDB AUTO_INCREMENT=307 DEFAULT CHARSET=utf8mb4 COMMENT='work作品';




