--"UniswapV2Factory": "0x401f40dc102a4f7cf9419ac884ed8e00bcd4ca98",
INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x401f40dc102a4f7cf9419ac884ed8e00bcd4ca98','0x7fbabee1ee0da0be8f120336e3c57ba3f192db4e2d35a5bb92a4ab7ef8bfe59f','0x75F970','http://172.31.17.151:9283/transaction/call',NULL,'2022-04-22 00:00:00','D4APairCreated',0,0,220);


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5622Fb509e29A804b45CCeb05a6D735407350526','0xad5c4648','0x75F970',NULL,NULL,'2022-04-22 00:00:00','wethAddress',0,0,220);


DELETE sub , blo FROM subscriber AS sub,
    block_height AS blo
WHERE
    sub.id = blo.sub_id
    AND sub.topics = '["0x4012961728e0bbfc6a344936c7fe045feb896ffd93c7594258129a04fcd30e95"]';


update subscribe set contract_address = '0x21B358F80B962B8aFa5C0EcB8E8e8877d9eA9633',filter_id = null,status=0 where contract_address = '0xFeB00135783CDa899aa02BBC5B4Fec4b51a2Cea6';

delete from subscribe where topics = '0x140bc2313a2670a47691808fc15120a7606016b5548f2a00cf24fda653989949';
delete from subscribe where topics = '0x653cdfdad74cc74920b2f8d7f5be1c9c468ea6f2476cfe5df6a4d67163fa3f85';
delete from subscribe where topics = '0x8b0d07dd4a894e7e06b7ad2e851b0561d78084f4a3c91b17677747bf6be13e52';
delete from subscribe where topics = '0xdb69a77c7f00775bb8033e49ce1d856add6069898c697c0031e62069bce9018a';
delete from subscribe where topics = '0x4012961728e0bbfc6a344936c7fe045feb896ffd93c7594258129a04fcd30e95';

--D4ATransfer("", "0x140bc2313a2670a47691808fc15120a7606016b5548f2a00cf24fda653989949"),
    D4AMint("", "0xe7f26cdf45159dd0c7ff634efbd5459ca530ae48e45ea47d006bb6ab8b31dc35"),
    D4ASync("", "0x0b0b2391a60d56c106458a58b1bc30475f99318d4b04b91cec7e2eba733680af"),
    D4ABurn("", "0x01ecf300005a210787cd5e5ca994087be9362c74c5ffef6c83475ec4fdebd9d2"),
    D4ASwap("", "0x28a3a211217061c19ad79ece1779dee92ed50532c4d35e5b2501c81d2010abfd");

--    D4ATransfer("D4ATransfer", "交易对的流动", false, SubscriberTypeEnum.EVENT, "d4aTransferChainServiceImpl"),
        D4AMint("D4AMint", "添加流动性", false, SubscriberTypeEnum.EVENT, "d4aMintChainServiceImpl"),
        D4ASync("D4ASync", "最新储备量", false, SubscriberTypeEnum.EVENT, "d4aSyncChainServiceImpl"),
        D4ABurn("D4ABurn", "移除流动性", false, SubscriberTypeEnum.EVENT, "d4aBurnChainServiceImpl"),
        D4ASwap("D4ASwap", "兑换交易", false, SubscriberTypeEnum.EVENT, "d4aSwapChainServiceImpl"),


INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5622Fb509e29A804b45CCeb05a6D735407350526','0xe7f26cdf45159dd0c7ff634efbd5459ca530ae48e45ea47d006bb6ab8b31dc35','0x75F970','http://172.31.17.151:9283/transaction/call',NULL,'2022-04-22 00:00:00','D4AMint',0,0,220);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5622Fb509e29A804b45CCeb05a6D735407350526','0x0b0b2391a60d56c106458a58b1bc30475f99318d4b04b91cec7e2eba733680af','0x75F970','http://172.31.17.151:9283/transaction/call',NULL,'2022-04-22 00:00:00','D4ASync',0,0,220);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5622Fb509e29A804b45CCeb05a6D735407350526','0x01ecf300005a210787cd5e5ca994087be9362c74c5ffef6c83475ec4fdebd9d2','0x75F970','http://172.31.17.151:9283/transaction/call',NULL,'2022-04-22 00:00:00','D4ABurn',0,0,220);

INSERT INTO `subscribe` (`contract_address`,`topics`,`from_block`,`receive_address`,`filter_id`,`create_time`,`trade_type`,`is_del`,`status`,`order_init`)
VALUES ('0x5622Fb509e29A804b45CCeb05a6D735407350526','0x28a3a211217061c19ad79ece1779dee92ed50532c4d35e5b2501c81d2010abfd','0x75F970','http://172.31.17.151:9283/transaction/call',NULL,'2022-04-22 00:00:00','D4ASwap',0,0,220);




