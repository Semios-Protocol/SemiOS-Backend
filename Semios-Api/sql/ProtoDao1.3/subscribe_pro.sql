CREATE TABLE `block_height` (
  `id` int NOT NULL AUTO_INCREMENT,
  `sub_id` int DEFAULT NULL,
  `from_block` varchar(45) DEFAULT NULL,
  `to_block` varchar(45) DEFAULT NULL,
  `filter_id` varchar(46) DEFAULT NULL,
  `modify_time` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='订阅区块高度表';


CREATE TABLE `sub_num_value` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'id',
  `address` varchar(64) DEFAULT NULL COMMENT '合约地址',
  `net_work` varchar(64) DEFAULT NULL COMMENT '网络类型',
  `topic` varchar(500) DEFAULT NULL COMMENT '订阅的方法',
  `block_height` varchar(32) DEFAULT NULL COMMENT '区块高度',
  `value` varchar(128) DEFAULT NULL COMMENT '区块上的值',
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `filter_id` int DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='数值类型订阅最新值';



CREATE TABLE `subscriber` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `network` varchar(45) NOT NULL COMMENT '网络Mainnet Ropsten',
  `from_block` varchar(45) DEFAULT NULL COMMENT '开始块高度 为空则从当前块开始',
  `address` varchar(45) DEFAULT NULL COMMENT '监听地址',
  `topics` varchar(500) DEFAULT NULL COMMENT '监听主题',
  `interval_time` int DEFAULT '10' COMMENT '间隔时间 秒s',
  `notice_url` varchar(300) DEFAULT NULL COMMENT 'noticeurl',
  `notice_err_times` int DEFAULT '0' COMMENT '通知异常次数超过10次则停止通知',
  `sub_status` int DEFAULT '0' COMMENT '监听状态 0-关闭 1-开启',
  `is_del` int DEFAULT '0' COMMENT '删除标识 0-未删除 1-已删除',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
  `notice_type` int DEFAULT '0' COMMENT '监听类型0-transaction 1-数值',
  `app_name` varchar(45) DEFAULT NULL COMMENT 'appname',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='订阅记录表';


CREATE TABLE `transaction` (
  `id` int NOT NULL AUTO_INCREMENT,
  `address` varchar(45) DEFAULT NULL,
  `block_hash` varchar(66) DEFAULT NULL,
  `block_number` varchar(45) DEFAULT NULL,
  `block_int_num` int DEFAULT NULL COMMENT 'blockNumber10进制',
  `data` text,
  `log_index` varchar(45) DEFAULT NULL,
  `removed` varchar(45) DEFAULT NULL,
  `topics` varchar(1000) DEFAULT NULL,
  `transaction_hash` varchar(66) DEFAULT NULL,
  `transaction_index` varchar(45) DEFAULT NULL,
  `sub_id` int DEFAULT NULL,
  `notice_times` int DEFAULT '0' COMMENT '通知次数',
  `notice_status` int DEFAULT '0' COMMENT '通知状态 0-未成功 1-已成功',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `block_timestamp` varchar(45) DEFAULT NULL COMMENT 'bolck',
  `app_name` varchar(45) DEFAULT NULL COMMENT 'appname',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=10 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='transaction';