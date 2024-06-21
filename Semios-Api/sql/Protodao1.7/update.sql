# dao表中添加字段
alter table `protodao`.dao add column pay_currency_type varchar(255) default null comment '支付货币类型';
alter table `protodao`.dao add column input_token_logo varchar(255) default null comment 'input token的logo地址'; #  从哪里获取
alter table `protodao`.dao add column input_token_address varchar(200) default null comment 'input token的address'; # 合约抛出，input token的address，为空的话默认为Eth
alter table `protodao`.dao add column input_token_decimals int(11) default null comment 'input token的decimals'; # input token的decimals

alter table `protodao`.dao add column canvas_created_whitelist_nft int(11) default 0 comment '是否开通创建canvas下某nft白名单  0关闭 1-开启';
alter table `protodao`.dao add column minter_works_whitelist_nft int(11) default 0 comment '是否开通erc721下某nft白名单  0关闭 1-开启';
alter table `protodao`.dao add column erc721_mint_cap_id int(11) default 0 comment '是否开启erc721下的某个nft铸造白名单  0关闭 1-开启';


-- dex服务修改
alter table dao4art.user_liquidity_statistics
    modify erc20_balance decimal(48, 18) null comment 'erc20ä½™é¢';



# 修改事件
# TradeTypeEnum.CreateProjectParamEmittedFour
# 0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b -->
# 0x25f02c4627085077d5f7f4249b2955a75b86fef48841796c699ce67742f00c4a
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b%';
update `subscribe` set topics='0x25f02c4627085077d5f7f4249b2955a75b86fef48841796c699ce67742f00c4a',filter_id = null,status=0 where topics like '%0xf1a52e281927c0a84fda64aa7d7219de0e85d7626e33068a2d8827a534f80c6b%';

# MintCapSet 事件修改
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c%';
update `subscribe` set topics='0x62285c51c0930afb5eecd599bbc38a6fbb7437a1a7c35693411877697d72b50f',filter_id = null,status=0 where topics like '%0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c%';

# WhitelistModified 事件修改
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec%';
update `subscribe` set topics='0x019cd1cc810dc2aacd4ef1720947a9f55bf58be7d83d6eeaf01330dd110531e8',filter_id = null,status=0 where topics like '%0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec%';


# TradeTypeEnum.CreateContinuousProjectParamEmitted
# 0x6aa2e30146ca7c4c3b3f243ce557791a1c6b189ac5ca56f12e8a3e6a324adce7 -->
# 0x19f1ef28c13e9bb7b435e2967c9b3a6f9cfe800a04df8b512fb4e048ebfa0526


# TradeTypeEnum.WhitelistModified
# 0xd78359fb71b50bade74b9b251e9b405728f05b904504917fa96a72456dd22eec -->
# 0x019cd1cc810dc2aacd4ef1720947a9f55bf58be7d83d6eeaf01330dd110531e8


# TradeTypeEnum.MintCapSet
# 0x3948b05591b6a76224011fe4e731ca76635e073a01faa544760e4493b404a96c -->
# 0x62285c51c0930afb5eecd599bbc38a6fbb7437a1a7c35693411877697d72b50f