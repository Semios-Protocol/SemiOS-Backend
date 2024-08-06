# add table `permissions_nft`
# 这个node的 ** 权限 现在绑定到了 ** nft上，
-- nft权限表
CREATE TABLE `protodao`.`node_permission_nft` (
                                                  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',

                                                  `dao_id` int(11) NOT NULL COMMENT 'dao id', # 可能是seed nodes id 或者 sub nodes id
                                                  `project_id` varchar(200) NOT NULL COMMENT 'dao project id', # 对应nodes的project id
                                                  `node_type` int(1) NOT NULL COMMENT 'node类型', # 1-seed nodes 2-sub nodes

                                                  `generate_work_id` int(11) NOT NULL COMMENT '创建nodes生成的work id', # 创建nodes生成的 work id
                                                  `generate_erc721_address` varchar(200) NOT NULL COMMENT 'erc721 address', # 创建nodes生成的 erc721 address
                                                  `generate_erc721_token_id` varchar(200) NOT NULL COMMENT 'erc721 token id', # 创建nodes生成的 应该都是0

                                                  `permissions_type` int(11) NOT NULL COMMENT '权限类型', # 权限类型 seed nodes 7种权限，sub nodes 5种权限
                                                  `permissions_dao_id` int(11) NOT NULL COMMENT '这个权限下的nft对应的dao id',
                                                  `permissions_work_id` int(11) NOT NULL COMMENT '这个权限下的nft对应的work id',
                                                  `permissions_erc721_address` varchar(200) NOT NULL COMMENT 'erc721 address',  # 当前权限对应的 erc721 address
                                                  `permissions_erc721_token_id` varchar(200) NOT NULL COMMENT 'erc721 token id', # 当前权限对应的 erc721 address

                                                  `transaction_hash` varchar(66) DEFAULT NULL COMMENT '记录的交易hash',

                                                  `is_valid` tinyint(1) NOT NULL DEFAULT 1 COMMENT '是否有效 1-有效 0-无效',
                                                  `is_del` tinyint(1) NOT NULL DEFAULT 0 COMMENT '是否删除 0-否 1-是',
                                                  `update_time` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
                                                  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                                                  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='nft权限表';



-- 添加新的事件 转移Edit Information权限（所有dao均有）
-- event DaoEditInformationNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0x2f42496343b0bf6886ffb703f087eea8f1a06b7802407bbb7f2974b05ff305fc
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0x2f42496343b0bf6886ffb703f087eea8f1a06b7802407bbb7f2974b05ff305fc','','http://172.31.31.55:9480/transaction/call',NULL,'DaoEditInformationNftOwnerSet',0,0,3);


-- 添加新的事件 转移Edit On-chain parameters权限（所有dao均有）
-- event DaoEditParameterNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0x70f896b18f07f19c3b8c713635192a57ac5d49729cd867aa5328d0be2cadea00
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0x70f896b18f07f19c3b8c713635192a57ac5d49729cd867aa5328d0be2cadea00','','http://172.31.31.55:9480/transaction/call',NULL,'DaoEditParameterNftOwnerSet',0,0,3);

-- 添加新的事件 转移Edit Strategies权限（所有dao均有）
-- event DaoEditStrategyNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0x75cac64a6b5dc64e5c863e66104f6ff3b7809cea055ba794027e25cdd7bbdf45
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0x75cac64a6b5dc64e5c863e66104f6ff3b7809cea055ba794027e25cdd7bbdf45','','http://172.31.31.55:9480/transaction/call',NULL,'DaoEditStrategyNftOwnerSet',0,0,3);


-- 添加新的事件 转移SubNodes Creator收益权限（所有dao均有）
-- event DaoRewardNftOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0x789f647231d97f01880a8e96caf0763f01497db63b0e66f5a93536a45583a70c
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0x789f647231d97f01880a8e96caf0763f01497db63b0e66f5a93536a45583a70c','','http://172.31.31.55:9480/transaction/call',NULL,'DaoRewardNftOwnerSet',0,0,3);


-- 添加新的事件 转移设置top-up账户分流权限（仅main dao）
-- event TreasurySetTopUpRatioOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0xfe13a4ee0df34165c526b06714fac432dc5ea4a6a8eb7c501538274616cc9c62
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0xfe13a4ee0df34165c526b06714fac432dc5ea4a6a8eb7c501538274616cc9c62','','http://172.31.31.55:9480/transaction/call',NULL,'TreasurySetTopUpRatioOwnerSet',0,0,3);


-- 添加新的事件 转移编辑seed nodes information的权限（仅main dao）
-- event TreasuryEditInformationOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0xf56fbe351490b7aefbaf51475fd6c70f3a07567e5c2c24696ff9d6c0519577ca
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0xf56fbe351490b7aefbaf51475fd6c70f3a07567e5c2c24696ff9d6c0519577ca','','http://172.31.31.55:9480/transaction/call',NULL,'TreasuryEditInformationOwnerSet',0,0,3);

-- 添加新的事件 转移国库的分配权限（仅main dao）
-- event TreasuryTransferAssetOwnerSet(bytes32 daoId, address nftAddress, uint256 tokenId)
-- 0xfb266fbf14421b044f89efe9ad6f9bede353c6bec592a53c4fabba037fc4b9ea
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xBCeF8B7AcB9bc414438B9EA49d86075D77b076C4','0xfb266fbf14421b044f89efe9ad6f9bede353c6bec592a53c4fabba037fc4b9ea','','http://172.31.31.55:9480/transaction/call',NULL,'TreasuryTransferAssetOwnerSet',0,0,3);

