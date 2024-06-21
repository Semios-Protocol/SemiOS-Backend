# 更新了创建dao的订阅流程,需要更新订阅表中的订阅事件

# 删除所有订阅服务表中的数据
delete from subscribe_pro.block_height where 1=1;
delete from subscribe_pro.sub_num_value where 1=1;
delete from subscribe_pro.subscriber where 1=1;
delete from subscribe_pro.transaction where 1=1;

# 主事件：CreateProjectParamEmittedFour  topics = '0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b';
# 删除创建dao主事件的其他订阅
delete from protodao.subscribe where topics = '0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7'; # protoDao创建扩展
delete from protodao.subscribe where topics = '0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693'; # 创建project
delete from protodao.subscribe where topics = '0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686'; # 创建canvas
delete from protodao.subscribe where topics = '0x8d5ea2d27d4f1e4d950c9e1187b29f34bea520f079834249e9dc40141f85aa53'; # 创建Pools
delete from protodao.subscribe where topics = '0xd3667753fa0651924c7260f11d6c97a9fed6546fd290520ec5564f244c5771c3'; # 创建Dao时用于给sub dao打款赠送的erc721的地址
delete from protodao.subscribe where topics = '0x08eeb17ea135873525c910d41ad046f125008bc45892a06cb78708b1092336a8'; # 创建Main dao 用于给国库打款赠送的erc721的地址
delete from protodao.subscribe where topics = '0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e'; # 创建splitter合约
# delete from protodao.subscribe where topics = '0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959'; # TradeTypeEnum.ChildrenSet
# delete from protodao.subscribe where topics = '0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d'; # TradeTypeEnum.RatioForFundingSet
# delete from protodao.subscribe where topics = '0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c'; # 修改黑白名单相关参数
# delete from protodao.subscribe where topics = '0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08'; # 进入minter黑名单
# delete from protodao.subscribe where topics = '0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66'; # 进入canvascreator黑名单
# delete from protodao.subscribe where topics = '0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec'; # 白名单修改
delete from protodao.subscribe where topics = '0x2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d'; # 授权
delete from protodao.subscribe where topics = '0xf6391f5c32d9c69d2a47ea670b442974b53935d1edc7fd64eb21e047a839171b'; # 取消授权


INSERT INTO protodao.subscribe (contract_address, topics, from_block, receive_address, filter_id, create_time, trade_type, is_del, status, order_init, interval_time) VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05', '0x62285c51c0930afb5eecd599bbc38a6fbb7437a1a7c35693411877697d72b50f', null, 'http://172.31.16.128:9480/transaction/call', NULL, NULL, 'MintCapSet', 0,0, 9, 10);
INSERT INTO protodao.subscribe (contract_address, topics, from_block, receive_address, filter_id, create_time, trade_type, is_del, status, order_init, interval_time) VALUES ('0x058Af087185bAdB181b879573C74E3cd1834DfcE', '0xed15a419ad9c29678e61cf5e5d96d1097630c3a80ecca3640c32dc148e014c08', null, 'http://172.31.16.128:9480/transaction/call', NULL, NULL, 'MinterBlacklisted',  0,0, 107, 10);
INSERT INTO protodao.subscribe (contract_address, topics, from_block, receive_address, filter_id, create_time, trade_type, is_del, status, order_init, interval_time) VALUES ('0x058Af087185bAdB181b879573C74E3cd1834DfcE', '0x1316604f664c0ceb1568ded46d6d8935095eeca4d0a54e8f68a18c109713fa66', null, 'http://172.31.16.128:9480/transaction/call', NULL, NULL, 'CanvasCreatorBlacklisted', 0,0, 108, 10);
INSERT INTO protodao.subscribe (contract_address, topics, from_block, receive_address, filter_id, create_time, trade_type, is_del, status, order_init, interval_time) VALUES ('0x058Af087185bAdB181b879573C74E3cd1834DfcE', '0x019cd1cc810dc2aacd4ef1720947a9f55bf58be7d83d6eeaf01330dd110531e8', null, 'http://172.31.16.128:9480/transaction/call', NULL, NULL, 'WhitelistModified',  0,0, 111, 10);

select contract_address from protodao.subscribe where trade_type = '';

INSERT INTO protodao.subscribe (contract_address, topics, from_block, receive_address, filter_id, create_time, trade_type, is_del, status, order_init, interval_time) VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05', '0x1166e5aeee44d00cb896cf709e93c58ae57d2ec75ce7044a3d1de22c977f8959', null, 'http://172.31.16.128:9480/transaction/call', NULL, NULL, 'ChildrenSet', 0,0, 10, 10);
INSERT INTO protodao.subscribe (contract_address, topics, from_block, receive_address, filter_id, create_time, trade_type, is_del, status, order_init, interval_time) VALUES ('0xA661DD93ABF98eED49938cF41Eec9014a9f79c05', '0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d', null, 'http://172.31.16.128:9480/transaction/call', NULL, NULL, 'RatioForFundingSet', 0,0, 10, 10);
