-- 记录国库交易
CREATE TABLE `protodao`.`treasury_transaction` (
                                        `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'id',
                                        `project_id` varchar(200) DEFAULT NULL COMMENT 'daoID', -- main dao project id

                                        -- 添加 收款sub dao project，合约抛出
                                        `transaction_hash` varchar(66) DEFAULT NULL COMMENT '记录的交易hash',
                                        `from_address` varchar(200) DEFAULT NULL COMMENT 'from的地址', -- 合约 granter
                                        `to_address` varchar(200) DEFAULT NULL COMMENT 'to的地址', -- 给国库打款，为国库的address---给 sub node打款，为node的erc20地址
                                        `block_number` varchar(66) DEFAULT NULL COMMENT '上链时间区块高度', -- 合约 grantBlock

                                        `transaction_type` int(1) DEFAULT NULL COMMENT '交易类型 0-给sub node打款 1-给国库打款',

                                        `generate_erc721_address` varchar(200) DEFAULT NULL COMMENT '赠送的NFT的721地址', -- 合约事件的erc721Address
                                        `generate_token_id` varchar(200) DEFAULT NULL COMMENT '赠送的NFT的token id', -- 合约事件的tokenId

                                        `amount` decimal(36,18) DEFAULT NULL COMMENT '打款或收款的amount', -- 合约 grantAmount

                                        -- 给sub node打款  transaction_type = 0 才有值
                                        `is_use_treasury` int(1) DEFAULT NULL COMMENT '是否通过国库打款', -- 合约 isUseTreasury
                                        `sub_dao_project_id` varchar(200) DEFAULT NULL COMMENT 'subDaoID', -- main dao project id
                                        `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                                        PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='国库交易表';



-- dao table 添加新的字段
alter table `protodao`.dao add column `treasury_erc20` varchar(200) DEFAULT NULL COMMENT '国库 对应的erc20 token地址'; -- 合约需要抛出相关信息
alter table `protodao`.dao add column `eth_token_royalty` decimal(16,6) DEFAULT 0 COMMENT 'eth解锁dao token的wallet比例(未开启wec20)';
alter table `protodao`.dao add column `token_eth_royalty` decimal(16,6) DEFAULT 0 COMMENT 'dao token解锁eth的wallet比例(开启erc20)';
-- 关于比例默认值的问题 将系列dao的默认值放在isTogetherDao=1的比例下面

alter table dao add column `dao_nft_erc721` varchar(200) DEFAULT NULL COMMENT '创建dao 生成的nft凭证721'; -- 合约需要抛出相关信息
alter table dao add column `grant_dao_nft_erc721` varchar(200) DEFAULT NULL COMMENT '创建dao，给sub dao 打款生成的erc721地址'; -- 合约需要抛出相关信息
alter table dao add column `grant_treasury_nft_erc721` varchar(200) DEFAULT NULL COMMENT '创建Main dao用于给国库打款赠送的erc721的地址'; -- 合约需要抛出相关信息



-- 事件修改  新建dao时 获取国库erc20地址。。
-- event NewPools(bytes32 daoId,address daoAssetPool,address daoRedeemPool,uint256 daoTopUpEthToRedeemPoolRatio, uint256 daoTopUpErc20ToTreasuryRatio,bool isThirdPartyToken);
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x88f566bf40d679b42ed2d0048a8bc81578fb0e246dac23b571e438044a5d5163%';
update `protodao`.`subscribe` set topics='0x8d5ea2d27d4f1e4d950c9e1187b29f34bea520f079834249e9dc40141f85aa53',filter_id = null,status=0 where topics like '%0x88f566bf40d679b42ed2d0048a8bc81578fb0e246dac23b571e438044a5d5163%';


-- 事件修改  在添加dao的时候事件修改 创建dao事件修改
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xfd230129167daa1b6256b88e7de931428fa7b35c4fe5cb93cc7b28add4deb401%';
update `protodao`.`subscribe` set topics='0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b',filter_id = null,status=0 where topics like '%0xfd230129167daa1b6256b88e7de931428fa7b35c4fe5cb93cc7b28add4deb401%';

-- 事件添加 ，用于给sub dao打款赠送的erc721的地址
-- event NewSemiDaoErc721Address(bytes32 daoId, address daoNft, address grantDaoNft);
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0xd3667753fa0651924c7260f11d6c97a9fed6546fd290520ec5564f244c5771c3','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewSemiDaoErc721Address',0,0,3);


-- 事件添加，只有main dao创建会抛出，记录国库地址，给国库打款的721地址和初始国库资金
-- event NewSemiTreasury(bytes32 daoId, address treasury, address grantTreasuryNft, uint256 initTokenSupply);
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0x08eeb17ea135873525c910d41ad046f125008bc45892a06cb78708b1092336a8','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewSemiTreasury',0,0,3);




-- 添加新的事件，给sub node打款事件
-- event NewSemiOsGrantAssetPoolNft(address erc721Address,uint256 tokenId,bytes32 daoId,address granter,uint256 grantAmount,bool isUseTreasury,uint256 grantBlock,address token);
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0x0509075f80cdea14ec28c1502071ee6f49da8ed96e40a22401f4d560b5411813','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewSemiOsGrantAssetPoolNft',0,0,3);


-- 添加新的事件，获取给国库 打款 信息
-- event NewSemiOsGrantTreasuryNft(address erc721Address,uint256 tokenId,bytes32 daoId,address granter,uint256 grantAmount,uint256 grantBlock,address token);
INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0xf7ba94813f5fd7204e9bc67e024cc47eac2487950353607b3e3f512c83596de9','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','NewSemiOsGrantTreasuryNft',0,0,3);


-- 下面的四个事件已经取消
-- 添加新的事件，修改比例默认值
-- event DefaultTopUpEthToRedeemPoolRatioSet(bytes32 daoId, uint256 ethToRedeemPoolRatio);
-- INSERT INTO `protodao`.`subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
-- VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0xb8fff1a50ecee491f6276980e05a8a976b2f2f619b05cf940fe0653f58e16a00','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DefaultTopUpEthToRedeemPoolRatioSet',0,0,3);

-- 添加新的事件，修改比例默认值
-- event DefaultTopUpErc20ToTreasuryRatioSet(bytes32 daoId, uint256 erc20ToTreasuryRatio);INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
-- INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
-- VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0x84b7f0ec9481c092c997dc96a2289928cca65fc7f7859949b7bd085f061e29df','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DefaultTopUpErc20ToTreasuryRatioSet',0,0,3);

-- 添加新的事件，修改某个dao下比例
-- event DaoTopUpErc20ToTreasuryRatioSet(bytes32 daoId, uint256 erc20ToTreasuryRatio);
-- INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
-- VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0xbc1fe30b7e1dcd6b519ebbab6763ca6f93f2872e3ebd8b6569f4eeef4663648f','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoTopUpErc20ToTreasuryRatioSet',0,0,3);

-- 添加新的事件，修改某个dao下比例
-- event DaoTopUpEthToRedeemPoolRatioSet(bytes32 daoId, uint256 ethToRedeemPoolRatio);
-- INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
-- VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0xa39c00f808213fa402939b8c49a3f945894fede54d1479a3e041d86e7b46e360','','http://172.31.16.128:9480/transaction/call',NULL,'2022-04-22 00:00:00','DaoTopUpEthToRedeemPoolRatioSet',0,0,3);

-- 修改比例汇总
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0x26b15987e8b5c3f3eade30d4775bf68b51d00042b41b48861ca199489881e093','','http://172.31.16.128:9480/transaction/call',NULL,'TopUpEthSplitRatioSet',0,0,3);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0x01db1017e94e3dd5318b6141da3775021fafe895c184ce30b736ab0a06b284ba','','http://172.31.16.128:9480/transaction/call',NULL,'TopUpErc20SplitRatioSet',0,0,3);



-- 下面为 readme 的 修改 事件
-- 监听dao领取代币事件
-- event PDClaimDaoCreatorReward(bytes32 daoId, address token, uint256 erc20Amount, uint256 ethAmount, address receiver);
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x3dc08e4906aa208d8fdfa834e277401cb5225e3b1cbbba95d272339e83f0ebaa%';
update `protodao`.`subscribe` set topics='0xeeb8b359b15aca07a16fbd5fb4846359bd6ed4bff6131953b5b5cb866f8d7b5b',filter_id = null,status=0 where topics like '%0x3dc08e4906aa208d8fdfa834e277401cb5225e3b1cbbba95d272339e83f0ebaa%';


-- 监听canvas领取代币和eth
-- event PDClaimCanvasReward(bytes32 daoId, bytes32 canvasId, address token, uint256 erc20Amount, uint256 ethAmount, address receiver);
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xce703b5c484c58e6dea0fa8768151a507705afbcd92d4b50b32093a0b0282b9d%';
update `protodao`.`subscribe` set topics='0x7afcf29a9e75e05ec104211e3deaea3190f9765041996c8256faca31de27dff6',filter_id = null,status=0 where topics like '%0xce703b5c484c58e6dea0fa8768151a507705afbcd92d4b50b32093a0b0282b9d%';


-- 监听minter领取代币事件
-- event PDClaimNftMinterReward(bytes32 daoId, address token, uint256 erc20Amount, uint256 ethAmount, address receiver);
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xa3b1b3dab0d710d977b133f4de916f2218d407931630ea0238827632a2f131f6%';
update `protodao`.`subscribe` set topics='0x7b4b7f47bc41f6ab49e31a450f9ef9d2952f23772ce18d6b31db8323d7367b02',filter_id = null,status=0 where topics like '%0xa3b1b3dab0d710d977b133f4de916f2218d407931630ea0238827632a2f131f6%';


-- 删除 无用的事件,,旧版本的领取代币事件，已经被上面的3个替换
DELETE from `protodao`.`subscribe` WHERE trade_type IN ('d4AClaimProjectERC20Reward','d4AClaimCanvasReward','d4AClaimNftMinterReward');


-- 添加新事件 用户mint work后的erc20分流收益
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05','0xd12c877b4de5fd85038ab99742f3f9d677c4bae61298714c4bcbea2836c47d59','','http://172.31.16.128:9480/transaction/call',NULL,'TopUpErc20Splitted',0,0,3);

