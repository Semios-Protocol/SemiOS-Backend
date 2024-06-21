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
update protodao.subscribe set contract_address='0x6199542a2a7594800d9b731f51bF02E8af130A7a' where contract_address = '0x6199542a2a7594800d9b731f51bF02E8af130A7a';    # D4ADrb
update protodao.subscribe set contract_address='0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901' where contract_address = '0xA661DD93ABF98eED49938cF41Eec9014a9f79c05';    # PDProtocol.proxy
update protodao.subscribe set contract_address='0x896089d413575936e757252612F2D28E491825C0' where contract_address = '0x058Af087185bAdB181b879573C74E3cd1834DfcE';    # PermissionControl.proxy
update protodao.subscribe set contract_address='0x209Ec7dEfea0D8823F5dbD101D42bDF3bd80fC68' where contract_address = '0x54E940B88617E2E6e7d95EC07b93A164B167C646';    # factories.D4ARoyaltySplitterFactory


# 删除所有获取dao周期的订阅
delete from protodao.subscribe where trade_type = 'getDaoCurrentRound';

# 删除所有不是本合约中的数据，比如 ETHTransfered,transfer,transferErc20..
delete from protodao.subscribe where contract_address not in ('0x6199542a2a7594800d9b731f51bF02E8af130A7a','0x5d1235dAe53F545b20B254CE2C8FE69aBB6bb901','0x896089d413575936e757252612F2D28E491825C0','0x209Ec7dEfea0D8823F5dbD101D42bDF3bd80fC68');

# 重新订阅所有的事件
update protodao.subscribe set from_block=null, filter_id = null,status=0 where 1=1;



#  dex服务清空数据
TRUNCATE table dao4art.erc20_liquidity;
TRUNCATE table dao4art.liquidity_daily_statistics;
TRUNCATE table dao4art.liquidity_price_record;
TRUNCATE table dao4art.liquidity_transaction;
TRUNCATE table dao4art.user_liquidity_statistics;

update dao4art.subscribe set from_block=null, filter_id = null,status=0 where 1=1;

#  更改dex服务配置中的合约

