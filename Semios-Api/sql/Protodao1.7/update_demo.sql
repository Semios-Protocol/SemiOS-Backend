# dao表中添加字段
alter table `protodao`.dao add column pay_currency_type varchar(255) default null comment '支付货币类型';
alter table `protodao`.dao add column input_token_logo varchar(255) default null comment 'input token的logo地址';
alter table `protodao`.dao add column input_token_address varchar(200) default null comment 'input token的address';
alter table `protodao`.dao add column input_token_decimals int(11) default null comment 'input token的decimals';

alter table `protodao`.dao add column canvas_created_whitelist_nft int(11) default 0 comment '是否开通创建canvas下某nft白名单  0关闭 1-开启';
alter table `protodao`.dao add column minter_works_whitelist_nft int(11) default 0 comment '是否开通erc721下某nft白名单  0关闭 1-开启';
alter table `protodao`.dao add column erc721_mint_cap_id int(11) default 0 comment '是否开启erc721下的某个nft铸造白名单  0关闭 1-开启';


-- 删除创建dao无用的订阅事件
delete from protodao.subscribe where topics = '0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7'; # protoDao创建扩展
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7%';

delete from protodao.subscribe where topics = '0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693'; # 创建project
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xe261677081adbd16b633ff5b5503a8b5b6548dbe78b1d135f05018a45b73c693%';

delete from protodao.subscribe where topics = '0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686'; # 创建canvas
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x8e79c3be756a5ba4a8733066ef0fbcdcb2087ec21dbcf8c4062a9b9532adb686%';

delete from protodao.subscribe where topics = '0x8d5ea2d27d4f1e4d950c9e1187b29f34bea520f079834249e9dc40141f85aa53'; # 创建Pools
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x8d5ea2d27d4f1e4d950c9e1187b29f34bea520f079834249e9dc40141f85aa53%';

delete from protodao.subscribe where topics = '0xd3667753fa0651924c7260f11d6c97a9fed6546fd290520ec5564f244c5771c3'; # 创建Dao时用于给sub dao打款赠送的erc721的地址
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xd3667753fa0651924c7260f11d6c97a9fed6546fd290520ec5564f244c5771c3%';

delete from protodao.subscribe where topics = '0x08eeb17ea135873525c910d41ad046f125008bc45892a06cb78708b1092336a8'; # 创建Main dao 用于给国库打款赠送的erc721的地址
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x08eeb17ea135873525c910d41ad046f125008bc45892a06cb78708b1092336a8%';

delete from protodao.subscribe where topics = '0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e'; # 创建splitter合约
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x2e286efede757a73dc1cb047b501080056d11c5852432baa1739a08ca434a75e%';


# 修改事件
# TradeTypeEnum.CreateProjectParamEmittedFour 主事件
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b%';
update `subscribe` set topics='0x25f02c4627085077d5f7f4249b2955a75b86fef48841796c699ce67742f00c4a',filter_id = null,status=0 where topics like '%0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b%';

# MintCapSet 事件修改
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c%';
update `subscribe` set topics='0x62285c51c0930afb5eecd599bbc38a6fbb7437a1a7c35693411877697d72b50f',filter_id = null,status=0 where topics like '%0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c%';

# WhitelistModified 事件修改
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec%';
update `subscribe` set topics='0x019cd1cc810dc2aacd4ef1720947a9f55bf58be7d83d6eeaf01330dd110531e8',filter_id = null,status=0 where topics like '%0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec%';


# 修改之前的数据
update `protodao`.dao set pay_currency_type = 'ETH' where 1=1;
update `protodao`.dao set input_token_logo = 'https://sepolia.etherscan.io/images/main/empty-token.png' where 1=1;
update `protodao`.dao set input_token_address = null where 1=1;
update `protodao`.dao set input_token_decimals = 18 where 1=1;
update `protodao`.dao set canvas_created_whitelist_nft = 0 where 1=1;
update `protodao`.dao set minter_works_whitelist_nft = 0 where 1=1;
update `protodao`.dao set erc721_mint_cap_id = 0 where 1=1;


-- dex服务修改
alter table dao4art.user_liquidity_statistics
    modify erc20_balance decimal(48, 18) null comment 'erc20Balance添加';