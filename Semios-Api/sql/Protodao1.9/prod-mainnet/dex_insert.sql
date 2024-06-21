# insert subscribe data
# TODO 合约地址需要调整


# TODO 回掉地址需要调整
# 0x73F3E7De7C526c519338abea054259D057F3558d->0x40082EEdca51A13E2910bBaDc1A0F87ce5730668
# 0x139288928a8d118bdad139d2d8f0d2a4960dba14->0x30C15072de34c87B99E0c6D0e6224C5FE98aDE72

# http://172.31.16.128:9483 -> http://172.31.20.253:9483
INSERT INTO dex_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (1, '0x40082EEdca51A13E2910bBaDc1A0F87ce5730668', '0x7fbabee1ee0da0be8f120336e3c57ba3f192db4e2d35a5bb92a4ab7ef8bfe59f', null, 'http://172.31.20.253:9483/transaction/call', null,  'D4APairCreated', 0, 1, 220, 10);
INSERT INTO dex_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (2, '0x30C15072de34c87B99E0c6D0e6224C5FE98aDE72', '0xe7f26cdf45159dd0c7ff634efbd5459ca530ae48e45ea47d006bb6ab8b31dc35', null, 'http://172.31.20.253:9483/transaction/call', null,  'D4AMint', 0, 1, 220, 10);
INSERT INTO dex_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (3, '0x30C15072de34c87B99E0c6D0e6224C5FE98aDE72', '0x0b0b2391a60d56c106458a58b1bc30475f99318d4b04b91cec7e2eba733680af', null, 'http://172.31.20.253:9483/transaction/call', null,  'D4ASync', 0, 1, 220, 10);
INSERT INTO dex_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (4, '0x30C15072de34c87B99E0c6D0e6224C5FE98aDE72', '0x01ecf300005a210787cd5e5ca994087be9362c74c5ffef6c83475ec4fdebd9d2', null, 'http://172.31.20.253:9483/transaction/call', null,  'D4ABurn', 0, 1, 220, 10);
INSERT INTO dex_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (5, '0x30C15072de34c87B99E0c6D0e6224C5FE98aDE72', '0x28a3a211217061c19ad79ece1779dee92ed50532c4d35e5b2501c81d2010abfd', null, 'http://172.31.20.253:9483/transaction/call', null,  'D4ASwap', 0, 1, 220, 10);
INSERT INTO dex_fork.subscribe (id, contract_address, topics, from_block, receive_address, filter_id,  trade_type, is_del, status, order_init, interval_time) VALUES (6, '0x30C15072de34c87B99E0c6D0e6224C5FE98aDE72', '0xad5c4648', null, null, null,  'wethAddress', 0, 1, 220, 10);
