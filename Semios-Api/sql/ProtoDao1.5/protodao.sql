-- 去除user_topup_harvest这张表：这张表绑定了用户拥有的eth和token数量
-- 1.5需求，将所有的top up挂在work下，而不是绑定到某个用户
-- 该work下所有top up 记录表
CREATE TABLE `work_topup_harvest` (
    `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',

    -- 铸造的nft记录 dao信息
    `dao_id` int(11) DEFAULT NULL COMMENT '铸造nft所属的 主dao id', -- 从 `dao` 中获取
    `project_id` varchar(200) DEFAULT NULL COMMENT '铸造nft所属的主dao的projectId', -- 从 `dao` 中获取

    -- 铸造的nft记录 work信息
    -- `work_id` int(11) DEFAULT NULL COMMENT '铸造nft的work主键ID',
    -- `work_number` int(11) DEFAULT NULL COMMENT '铸造nft的work编号',
    -- `round_drb` int(11) DEFAULT NULL COMMENT '铸造nft的分配时的drb周期',

    -- 铸造的nft余额
    `erc20_amount` decimal(36,18) DEFAULT NULL COMMENT '铸造nft的erc20余额',
    `eth_amount` decimal(36,18) DEFAULT NULL COMMENT '铸造nft的eth余额',

    -- 通过哪个nft来生成新nft  --监听铸造ntf事件来获取
    -- `mount_dao_id` int(11) DEFAULT NULL COMMENT '消费的dao id',
    `mount_erc721_address` varchar(200) DEFAULT NULL COMMENT '消费dao的project对应的erc721address地址', -- 合约事件的erc721 address（待确认：抛出的信息为新nft的还是消费的nft）
    `mount_work_id` int(11) DEFAULT NULL COMMENT '消费的work id',
    `mount_work_number` int(11) DEFAULT NULL COMMENT '消费的work编号', -- 合约事件的tokenId （待确认：抛出的信息为新nft的还是消费的nft）

    `is_del` int(11) DEFAULT '0' COMMENT '0-未删除 1-已删除',
    `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
PRIMARY KEY (`id`),
UNIQUE KEY `dao_id_mount_work_id` (`dao_id`,`mount_work_id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='topup模式work下的eth和token的数量';

-- 修改work表添加锁定字段
alter table work add column `lock_status` int(1) DEFAULT 0 COMMENT '锁定状态 0-未锁定 1-已锁定';  -- 监听事件来锁定nft （待确认：将锁定状态修改为未锁定状态的时机？定时任务？）
alter table work add column `lock_start_block` varchar(45) DEFAULT NULL COMMENT '锁定开始的区块高度';  -- 合约抛出的 lockStartBlock
alter table work add column  `lock_duration_block` varchar(45) DEFAULT NULL COMMENT '锁定的区块高度'; -- 合约抛出的 duration
alter table work add column  `mount_work_id` int(11) DEFAULT NULL COMMENT '绑定的workId'; -- 铸造nft时 绑定的work id

alter table dao add column `dao_restart_block` varchar(60) DEFAULT NULL COMMENT 'dao重启后的区块高度';


-- 添加新的事件，获取锁定信息
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x82b305a1F65418b337017e6B314aFad72EF2391A','0xfa53e89c71ce0061bedbb1bcc28303df90f9a91aa1bf21f1167886beb715b250',NULL,'http://172.31.16.128:9480/transaction/call',NULL,NULL,'TopUpNftLock',0,0,3);

-- 事件更新，mintNft事件添加nft结构体
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x5a2ad185679629c6f79a7ca8c3b2f337d1c8bce787a38af1b8a6b798a9d01e89%';
update `subscribe` set topics='0x1ce7903c69a8018cfe39951cc5e4afa327100389f4991ed7a6aa8f4083a6e33d',filter_id = null,status=0 where topics like '%0x5a2ad185679629c6f79a7ca8c3b2f337d1c8bce787a38af1b8a6b798a9d01e89%';
