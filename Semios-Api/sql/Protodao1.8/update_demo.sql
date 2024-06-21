# 1.8 版本所有数据需要清空

# 订阅服务
# 清空所有订阅表中的数据
delete from subscribe_pro.block_height where 1=1;
delete from subscribe_pro.sub_num_value where 1=1;
delete from subscribe_pro.subscriber where 1=1;
delete from subscribe_pro.transaction where 1=1;


# api服务
# 清空所有数据, 不包括订阅表
delete from protodao.canvas where 1=1;
delete from protodao.canvas_drb_statistics where 1=1;
delete from protodao.dao where 1=1;
delete from protodao.dao_allocation_amount where 1=1;
delete from protodao.dao_allocation_strategy where 1=1;
delete from protodao.dao_append_token_log where 1=1;
delete from protodao.dao_daily_statistics where 1=1;
delete from protodao.dao_drb_statistics where 1=1;
delete from protodao.dao_strategy where 1=1;
delete from protodao.favorites where 1=1;
delete from protodao.shutdown_record where 1=1;
delete from protodao.subscribe_bak where 1=1;
delete from protodao.token_received_record where 1=1;
delete from protodao.treasury_transaction where 1=1;
delete from protodao.user where 1=1;
delete from protodao.user_harvest_token where 1=1;
delete from protodao.user_topup_harvest where 1=1;
delete from protodao.white_list where 1=1;
delete from protodao.work where 1=1;
delete from protodao.work_topup_harvest where 1=1;


# 更新合约地址
update protodao.subscribe set contract_address='0x74d6aAca0fA8213F515A50995Aefbf47DA5b8A89' where contract_address = '0x74d6aAca0fA8213F515A50995Aefbf47DA5b8A89';    # D4ADrb
update protodao.subscribe set contract_address='0xC1634B63DbF98d844B6467B9e5290ce8b9fD0445' where contract_address = '0xDa1df2441A19c68C80Ab4Fffc02d10bA21666d0A';    # PDProtocol.proxy
update protodao.subscribe set contract_address='0xD0437f2CF0140f93e58B90185BC42A1B4CBb124e' where contract_address = '0xE62d865140692d4D027F557aCD844cD31D0b7737';    # PermissionControl.proxy
update protodao.subscribe set contract_address='0x79CC2E93E2607f840f0Ed7A944EE630aA793FB09' where contract_address = '0x9A25AaC8c093166513FcF267361A759537110331';    # factories.D4ARoyaltySplitterFactory


# 删除所有获取dao周期的订阅
delete from protodao.subscribe where trade_type = 'getDaoCurrentRound';

# 删除所有不是本合约中的数据，比如 ETHTransfered,transfer,transferErc20..
delete from protodao.subscribe where contract_address not in ('0x74d6aAca0fA8213F515A50995Aefbf47DA5b8A89','0xC1634B63DbF98d844B6467B9e5290ce8b9fD0445','0xD0437f2CF0140f93e58B90185BC42A1B4CBb124e','0x79CC2E93E2607f840f0Ed7A944EE630aA793FB09');

# 重新订阅所有的事件
update protodao.subscribe set from_block=null, filter_id = null,status=0 where 1=1;



#  dex服务清空数据
TRUNCATE table dao4art_dex.erc20_liquidity;
TRUNCATE table dao4art_dex.liquidity_daily_statistics;
TRUNCATE table dao4art_dex.liquidity_price_record;
TRUNCATE table dao4art_dex.liquidity_transaction;
TRUNCATE table dao4art_dex.user_liquidity_statistics;

update dao4art_dex.subscribe set from_block=null, filter_id = null,status=0 where 1=1;

#  更改dex服务配置中的合约

