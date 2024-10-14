CREATE SCHEMA `dex` DEFAULT CHARACTER SET utf8mb4;

create table dex.erc20_liquidity
(
    id                      int auto_increment comment 'id'
        primary key,
    dao_id                  int                                null comment 'dao_id',
    erc20_address           varchar(128)                       null comment 'erc20地址',
    erc20_name              varchar(50)                        null comment 'erc20名称',
    erc20_symbol            varchar(50)                        null comment 'erc20 symbol',
    erc20_block_time        bigint unsigned                    null comment 'erc20上链时间',
    eth_address             varchar(128)                       null comment 'eth地址',
    pair_address            varchar(128)                       null comment '交易对地址',
    pair_balance            decimal(36, 18)                    null comment 'pair token总余额',
    erc20_order             int(2)   default 0                 not null comment '创建交易对时erc20的顺序为第0个还是第1个',
    erc20_balance           decimal(36, 18)                    null comment 'erc20总余额',
    eth_balance             decimal(36, 18)                    null comment 'eth总余额',
    transaction_hash        varchar(128)                       null comment '交易对开通链上hash',
    block_time              bigint unsigned                    null comment '交易对开通上链时间',
    last_swap_hash          varchar(300)                       null comment '最近一次比例变更的hash',
    create_address          varchar(128)                       null comment '创建者用户地址',
    is_del                  int(2)   default 0                 not null comment '是否删除 0-未删除 1-已删除',
    create_time             datetime default CURRENT_TIMESTAMP not null comment '创建时间',
    modify_time             datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    royalty_fee_modify_hash varchar(128)                       null comment 'ERC20的版税收益修改的交易hash',
    royalty_fee             decimal(16, 6)                     null comment 'ERC20的版税收益',
    project_id              varchar(64)                        null comment 'projectId',
    dao_status              int      default 0                 null comment 'dao状态 0-未开始 1-已开始',
    dao_version             int      default 1                 null comment 'dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本',
    erc721_address          varchar(128)                       null comment 'erc721地址',
    app_source              int      default 1                 null comment 'app来源 1-dao4art 2-protodao',
    constraint erc20_address
        unique (erc20_address)
)
    comment 'erc20流通性表';

create table dex.liquidity_daily_statistics
(
    id                      int auto_increment comment 'id'
        primary key,
    erc20_address           varchar(128)                       null comment 'erc20地址',
    drb_number              int                                null comment '当前drb的值',
    erc20_amount            decimal(36, 18)                    null comment '交易erc总20数量',
    eth_amount              decimal(36, 18)                    null comment '交易eth总数量',
    eth_swapped_rate        decimal(36, 18)                    null comment '兑换eth比例',
    erc20_swapped_rate      decimal(36, 18)                    null comment '兑换erc20比例',
    apy                     decimal(36, 18)                    null comment 'APY',
    asset_pool_total_amount decimal(36, 18)                    null comment '资金池内总金额',
    asset_pool_income       decimal(36, 18)                    null comment '资金池收入',
    asset_pool_cost         decimal(36, 18)                    null comment '资金池支出',
    asset_pool_variation    decimal(36, 18)                    null comment '资金池变化量',
    record_time             bigint unsigned                    not null comment '记录时间戳',
    modify_time             datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    burn_price              decimal(36, 18)                    null comment 'burnPrice',
    constraint erc20_address_time
        unique (erc20_address, record_time)
)
    comment 'erc20流通性每日零点统计数据';

create table dex.liquidity_price_record
(
    id             int auto_increment comment 'id'
        primary key,
    type           int                                null comment '类型 0-swap交易量 1-burn交易量',
    erc20_address  varchar(128)                       null comment 'erc20地址',
    erc20_amount   decimal(36, 18)                    null comment '交易erc总20数量',
    eth_amount     decimal(36, 18)                    null comment '交易eth总数量',
    trading_volume decimal(36, 18)                    null comment '交易量',
    price          decimal(36, 18)                    null comment '交易价格 都是token兑换eth价格',
    record_time    bigint unsigned                    not null comment '记录时间戳',
    modify_time    datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint erc20_address_time
        unique (erc20_address, record_time, type)
)
    comment 'erc20流通性价格变更记录';

create table dex.liquidity_transaction
(
    id                int auto_increment comment 'id'
        primary key,
    erc20_address     varchar(128)                       null comment 'erc20地址',
    erc20_name        varchar(50)                        null comment 'erc20名称',
    erc20_symbol      varchar(50)                        null comment 'erc20Symbol',
    trade_type        int(2)                             not null comment '交易类型1-swapErc20 2-swapEth 3-add 4-remove 5-burn',
    transaction_hash  varchar(128)                       null comment '交易链上hash',
    block_time        bigint unsigned                    null comment '上链时间',
    in_token_amount   decimal(36, 18)                    null comment '交易时使用的代币 add、remove、burn固定是erc20 swapForEth为erc20 swapForToken为eth数量',
    out_token_amount  decimal(36, 18)                    null comment '交易时兑换出来的代币 add、remove、burn固定是eth swapForEth为eth swapForToken为erc20数量',
    erc20_balance     decimal(36, 18)                    null comment 'erc20总余额',
    eth_balance       decimal(36, 18)                    null comment 'eth总余额',
    eth_amount        decimal(36, 18)                    null comment '本次eth交易量',
    pool_token_amount decimal(36, 18)                    null comment '添加和减少流动性时的pool token数量',
    user_address      varchar(128)                       null comment '交易的用户地址',
    create_time       datetime default CURRENT_TIMESTAMP not null comment '创建时间',
    modify_time       datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint erc20_address_transaction_type
        unique (erc20_address, transaction_hash, trade_type)
)
    comment '流通性交易表';

create table dex.subscribe
(
    id               int auto_increment comment 'id'
        primary key,
    contract_address varchar(66)                        null comment '合约地址',
    topics           varchar(500)                       null comment '订阅的主题或者方法名',
    from_block       varchar(66)                        null comment '开始块高度',
    receive_address  varchar(66)                        null comment '接收回调地址',
    filter_id        varchar(2000)                      null comment '订阅ID',
    create_time      datetime default CURRENT_TIMESTAMP null comment '创建时间',
    trade_type       varchar(45)                        null,
    is_del           int      default 0                 null,
    status           int      default 0                 null,
    order_init       int                                null,
    interval_time    int      default 10                null comment '订阅时间',
    constraint UNIQUE_contract_address_topics
        unique (contract_address, topics, trade_type)
)
    comment '订阅记录';

create table dex.user_liquidity_statistics
(
    id                 int auto_increment comment 'id'
        primary key,
    erc20_address      varchar(128)                       null comment 'erc20地址',
    user_address       varchar(128)                       null comment '用户地址',
    user_id            varchar(128)                       null comment '用户id',
    user_logo          varchar(128)                       null comment '用户logo',
    is_contract        tinyint                            null comment '是否为合约地址 0-否 1-是',
    erc20_balance      decimal(48, 18)                    null comment 'erc20总量',
    pool_token         decimal(36, 18)                    null comment 'pool token数量',
    create_time        datetime default CURRENT_TIMESTAMP not null comment '创建时间',
    modify_time        datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    sync_erc20_balance tinyint  default 0                 null comment '是否需要同步erc20Balance 0-否 1-是',
    constraint user_erc20_address
        unique (erc20_address, user_address)
)
    comment '用户代币拥有量';

