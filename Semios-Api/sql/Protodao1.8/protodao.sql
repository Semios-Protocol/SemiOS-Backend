# work_topup_harvest 表中添加字段
alter table protodao.work_topup_harvest add `work_id` int(11) default null comment '新生成的nft' after id;
-- nft 合约挂帐的数量
alter table protodao.work_topup_harvest add `input_token_amount` decimal(36,18) DEFAULT NULL COMMENT 'input token的挂帐数量 ' after work_id;
alter table protodao.work_topup_harvest add `output_token_amount` decimal(36,18) DEFAULT NULL COMMENT 'out put token的挂帐数量' after input_token_amount;


# add table `plan`
-- 激励计划表
CREATE TABLE `protodao`.`incentive_plan` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
    `dao_id` int(11) NOT NULL COMMENT '聚合dao id',
    `project_id` varchar(200) NOT NULL COMMENT '聚合dao project id',
    `plan_code` varchar(200) NOT NULL COMMENT '合约生成的planId',
    `plan_number` int(11) NOT NULL COMMENT '后端生成的plan Number序号',
    `transaction_hash` varchar(66) DEFAULT NULL COMMENT '记录的交易hash',
    `current_round` int(11) NOT NULL COMMENT '当前周期',

    `plan_name` varchar(200) DEFAULT NULL COMMENT 'plan名称', -- 1.8版本暂不赋值，写死
    `plan_logo_url` varchar(300) DEFAULT NULL COMMENT 'logo url', -- 1.8版本暂不赋值，写死
    `incentive_type` int(11) NOT NULL COMMENT '计算激励贡献度的类型 1-input token 2-output token',
    `input_token_symbol` varchar(200) NOT NULL COMMENT 'input token symbol',
    `output_token_symbol` varchar(200) NOT NULL COMMENT 'output token symbol',

    `start_date` date NOT NULL COMMENT '开始时间',
    `start_block` varchar(66) NOT NULL COMMENT '开始区块高度',

    `amount_source` varchar(200) NOT NULL COMMENT '奖励来源 1-国库 2-钱包',
    `incentive_amount` decimal(36,18) NOT NULL COMMENT '计划激励金额',
    `remaining_token` decimal(36,18) NOT NULL COMMENT '剩余token数量',
    `plan_block_window` int(11) NOT NULL COMMENT 'plan的周期数量',
    `duration` int(11) NOT NULL COMMENT '每个周期的周期持续时间，单位hours',
    `incentive_status` int(11) NOT NULL COMMENT '计划状态  1-已创建未开始 2-已开始 3-已结束 4-已停机',

    `plan_uri` varchar(400) DEFAULT NULL COMMENT 'plan的uri',
    `create_by` varchar(66) NOT NULL COMMENT 'plan create user address',
    `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    PRIMARY KEY (`id`)

) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='激励计划表';


# plan结束后需要记录
create table `protodao`.`nft_reward_allocation`(
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
    `dao_id` int(11) NOT NULL COMMENT '聚合dao id',
    `project_id` varchar(200) NOT NULL COMMENT '聚合dao project id',
    `work_id` int(11) NOT NULL COMMENT 'nft的work id',
    `erc721_address` varchar(200) NOT NULL COMMENT 'erc721 address',
    `work_number` int(11) NOT NULL COMMENT 'work number',

    -- plan 的结束后的奖励
    `plan_code` varchar(200) DEFAULT NULL COMMENT '合约生成的planId',
    `plan_reward_amount` decimal(36,18) DEFAULT NULL COMMENT '可以collect的数量',

    `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    PRIMARY KEY (`id`)

) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='nft奖励记录表';

# collect流水记录
create table `protodao`.`collect_record`(
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
    `dao_id` int(11) NOT NULL COMMENT '聚合dao id',
    `project_id` varchar(200) NOT NULL COMMENT '聚合dao project id',

    `plan_code` varchar(200) NOT NULL COMMENT '合约生成的planId',

    `work_id` int(11) NOT NULL COMMENT 'nft的work id',
    `erc721_address` varchar(200) NOT NULL COMMENT 'erc721 address',
    `work_number` int(11) NOT NULL COMMENT 'work number',

    `transaction_hash` varchar(66) DEFAULT NULL COMMENT '记录的交易hash',
    `received_address` varchar(200) NOT NULL COMMENT '领取的address',
    `collect_amount` decimal(36,18) DEFAULT NULL COMMENT '领取的collect的数量',

    -- 交易发起人
    `create_by` varchar(66) NOT NULL COMMENT 'collect交易发起方',
    `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    PRIMARY KEY (`id`)
)ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='nft plan奖励分配记录';


-- 添加新的事件 NewSemiOsPlan
-- event NewSemiOsPlan(bytes32 planId,bytes32 daoId,uint256 startBlock,uint256 duration,uint256 totalRounds,uint256 totalReward,address rewardToken,uint8 planTemplateType,bool io,address owner,bool useTreasuryt);
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901','0x6aa2104d183ed7fffe1e940387d0c32625ecd6a56f93f815bac650cb373ef1af','','http://172.31.16.128:9480/transaction/call',NULL,'NewSemiOsPlan',0,0,3);

-- 添加本地订阅事件 获取每个plan的周期或状态
# INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
# VALUES ('0x6Db6d6b4C2eA88229edC698741a0E90F12f43E2C','0x3d5d910b','','http://172.31.16.128:9480/method/call',NULL,'planCurrentRound',0,0,3);


-- 添加订阅事件 plan追加token
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901','0xbbb2633759ec4150dc2586285718c49f18853666eff18aad457c64ad8f196f7d','','http://172.31.16.128:9480/transaction/call',NULL,'PlanTotalRewardAdded',0,0,3);


-- 添加订阅事件 用户Claimed奖励
# INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
# VALUES ('0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901','0x6df0d30ccd4f96ceb372a7eee962371f89463b963d4970962dea4a6646185d09','','http://172.31.16.128:9480/transaction/call',NULL,'PlanRewardClaimed',0,0,3);

-- 添加订阅事件 用户Claimed奖励的信号事件
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901','0xfbe0524fbfedf9d86084e1fe0061e119a6decd7bbbeb73744bd1bd7318e6ad76','','http://172.31.16.128:9480/transaction/call',NULL,'PlanRewardClaimSignal',0,0,3);


-- TopUpAccountUpdated链上余额变更
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901','0xfc1f5941de2e389cc0deb38e38420424a2e876572dea8cfb2a297b39b022848a','','http://172.31.16.128:9480/transaction/call',NULL,'TopUpAccountUpdated',0,0,3);
