# 数据全部清除，表结构从test中取
CREATE SCHEMA `protodao_fork` DEFAULT CHARACTER SET utf8mb4;

create table if not exists protodao_fork.canvas
(
    id                 int auto_increment comment 'id'
        primary key,
    canvas_name        varchar(200)                                 null comment 'canvas名称',
    canvas_description text                                         null comment 'canvas描述信息',
    canvas_logo        varchar(400)                                 null comment 'canvas的logo地址',
    canvas_number      int                                          null comment 'cavans的编号',
    dao_id             int                                          null comment 'dao的id',
    project_id         varchar(200)                                 null comment 'dao的projectid',
    dao_number         int                                          null comment '所属dao的编号',
    owner_address      varchar(42)                                  null comment 'canvas创建者地址',
    transaction_hash   varchar(66)                                  null comment 'canvas创建的交易hash',
    opensea_link       varchar(400)                                 null comment 'opensea链接',
    twitter_link       varchar(400)                                 null comment 'Twitter地址',
    discord_link       varchar(400)                                 null comment 'discord地址',
    block_time         varchar(45)                                  null comment 'Canvas创建的区块时间´',
    current_price      decimal(30, 6)                               null comment '当前铸造价格',
    block_number       varchar(20)                                  null comment 'Canvas创建的区块号',
    canvas_id          varchar(200)                                 null comment 'canvas创建时的canvasid',
    drb_number         int                                          null comment 'canvas创建时的drb号',
    total_drb_number   int             default 0                    null comment 'canvas创建剩余的drb总数',
    dao_floor_price    decimal(16, 6)                               null comment '所属dao的地板价',
    canvas_uri         varchar(400)                                 null comment 'canvas的uri地址',
    received_token     decimal(36, 18)                              null comment '已领取token数量',
    unclaimed_token    decimal(36, 18)                              null comment '未领取token数量',
    rest_drb           int                                          null comment '当前计算的drb，每次drb变化了获取最新的铸造价格',
    favorite_amount    int             default 0                    null comment 'canvas收藏数',
    dao_status         int             default 2                    null comment '2-已开始3-已结束',
    canvas_status      int             default 0                    null comment '0-未创建1-已创建 2-已停机',
    swap_token         decimal(36, 18)                              null comment '已兑换token数量',
    swap_eth           decimal(36, 18)                              null comment '已兑换eht数量',
    transfer_token     decimal(36, 18)                              null comment 'transfer token',
    create_time        datetime        default CURRENT_TIMESTAMP    null comment '创建时间',
    update_time        datetime        default CURRENT_TIMESTAMP    null on update CURRENT_TIMESTAMP comment '更新时间',
    social_links       text                                         null comment '社交链接 多个用逗号分隔',
    dao_symbol         varchar(45)                                  null comment 'dao symbol',
    royalty_token      decimal(16, 6)                               null comment 'canvas设置的royalty token 比例 默认为空',
    received_eth       decimal(36, 18) default 0.000000000000000000 null comment '已领取的eth数量',
    unclaimed_eth      decimal(36, 18) default 0.000000000000000000 null comment '未领取eth数量',
    constraint dao_canvas_number
        unique (dao_number, canvas_number)
)
    comment 'canvas画布' charset = utf8mb4;

create table if not exists protodao_fork.canvas_drb_statistics
(
    id                  int auto_increment comment 'id'
        primary key,
    dao_id              int                                null comment 'dao id',
    canvas_id           int                                null comment 'Canvas id',
    project_id          varchar(120)                       null comment 'dao的projectId',
    drb_number          int                                null comment 'drb号',
    mint_price          decimal(16, 6)                     null comment '当前铸造价格',
    drb_vol             decimal(16, 6)                     null comment 'Canvas在当前区块铸造费用总和',
    seven_day_drb_vol   decimal(16, 6)                     null comment '7天drbvol总和',
    total_vol           decimal(16, 6)                     null comment 'Canvas在本区块和之前所有区块铸造总费用',
    ntvr                decimal(16, 6)                     null comment 'Canvas在本区块铸造总费用占DAO在本区块铸造总费用的占比',
    dao_reward          decimal(16, 6)                     null comment 'Canvas在该区块结束后累计收到的ERC20数量',
    dre                 decimal(16, 6)                     null comment 'Mint Revenue+未来收益',
    owners              int                                null comment 'Canvas在该区块结束时所有NFT所在地址的私钥拥有者去重计数',
    nft                 int                                null comment 'Canvas在该区块结束时所有NFT的数量',
    work_amount         int                                null comment 'Canvas在该区块结束时所有work的数量',
    seven_day_ntrv      decimal(16, 6)                     null comment 'Canvas本区块加上六个区块的铸造总费用占DAO在本区块加上六个区块铸造总费用的占比',
    mint_revenue        decimal(16, 6)                     null comment 'Canvas的铸造总收入',
    status              int                                null comment '0-未计算 1-计算中 2-计算完成',
    times               int      default 0                 null comment '计算失败次数',
    mint_revenue_ex_tax decimal(16, 6)                     null comment 'Canvas的铸造总收入*0.675',
    drb_vol_ex_tax      decimal(16, 6)                     null comment 'Canvas在当前区块铸造费用总和 除税 Canvas在当前区块铸造费用总和*0.675',
    dao_drb_vol_ex_tax  decimal(16, 6)                     null comment 'Dao当前区块铸造费用总和 除税 Dao的当前canvas在当前区块铸造费用总和*0.3',
    create_time         datetime default CURRENT_TIMESTAMP null comment '创建时间',
    constraint dao_drb_number
        unique (dao_id, canvas_id, drb_number)
)
    comment 'canvas在drb的统计信息' charset = utf8mb4;

create table if not exists protodao_fork.collect_record
(
    id               int auto_increment comment 'id'
        primary key,
    dao_id           int                                     not null comment '聚合dao id',
    project_id       varchar(200) collate utf8mb4_unicode_ci not null comment '聚合dao project id',
    plan_code        varchar(200) collate utf8mb4_unicode_ci not null comment '合约生成的planId',
    work_id          int                                     not null comment 'nft的work id',
    erc721_address   varchar(200) collate utf8mb4_unicode_ci not null comment 'erc721 address',
    work_number      int                                     not null comment 'work number',
    transaction_hash varchar(66) collate utf8mb4_unicode_ci  null comment '记录的交易hash',
    received_address varchar(200) collate utf8mb4_unicode_ci not null comment '领取的address',
    collect_amount   decimal(36, 18)                         null comment '领取的collect的数量',
    create_by        varchar(66) collate utf8mb4_unicode_ci  not null comment 'collect交易发起方',
    create_time      datetime default CURRENT_TIMESTAMP      null comment '创建时间'
)
    comment 'nft plan奖励分配记录' charset = utf8mb4;

create table if not exists protodao_fork.dao
(
    id                               int auto_increment comment 'id'
        primary key,
    dao_name                         varchar(200)                                 null comment 'DAO名称',
    dao_manitesto                    text                                         null comment 'DAO宣言',
    dao_description                  text                                         null comment 'DAO介绍',
    dao_logo_url                     varchar(300)                                 null comment 'DAOlogo地址',
    dao_bg_banner                    varchar(300)                                 null comment 'DAO背景图片地址',
    dao_start_date                   date                                         null comment 'DAO开始日期',
    dao_start_drb                    int                                          null comment 'dao开始drb',
    total_nft_casting                int                                          null comment '最多可铸造的nft数量',
    dao_mint_window                  int                                          null comment 'DAO周期',
    dao_floor_price                  decimal(16, 6)                               null comment 'DAO地板价 DAO Floor Price增加0ETH的选项',
    dao_create_fee                   float                                        null comment 'Dao创建手续费',
    opensea_link                     varchar(400)                                 null comment 'opensea链接地址',
    twitter_link                     varchar(400)                                 null comment 'Twitter链接地址',
    discord_link                     varchar(400)                                 null comment 'Discord链接地址',
    dao_number                       int                                          null comment 'dao编号',
    dao_status                       int                                          null comment 'Dao状态0-未创建1-已创建未开始2-已开始3-已结束 4-已停机',
    block_time                       varchar(45)                                  null comment '上链时间',
    transaction_hash                 varchar(66)                                  null comment 'dao创建的交易hash',
    owner_address                    varchar(42)                                  null comment 'Dao拥有者地址',
    dao_uri                          varchar(400)                                 null comment 'dao的uri地址',
    drb_number                       int                                          null comment 'Dao创建时的drb区块',
    block_number                     varchar(45)                                  null comment 'Dao创建的链上区块号',
    project_id                       varchar(200)                                 null comment 'dao的projectid',
    received_token                   decimal(36, 18)                              null comment '已领取token数量',
    unclaimed_token                  decimal(36, 18)                              null comment '未领取token数量',
    fee_pool                         varchar(200)                                 null comment 'project对应的asset pool 地址',
    erc20_token                      varchar(200)                                 null comment 'project对应的erc20 token地址',
    erc721_token                     varchar(200)                                 null comment 'project对应的erc721 token地址',
    royalty_fee                      varchar(200)                                 null comment '二次售卖印花税设置',
    erc20_total_supply               varchar(200)                                 null comment '发放erc20总数',
    favorite_amount                  int             default 0                    null comment 'dao收藏数',
    canvas_floor_price               decimal(16, 6)                               null comment 'dao当前canvas最低价格',
    eth_amount                       decimal(36, 18)                              null comment 'eth余额--已废弃',
    swap_token                       decimal(36, 18)                              null comment '已兑换token数量',
    swap_eth                         decimal(36, 18)                              null comment '已兑换eht数量',
    dao_asset_pool                   decimal(36, 18)                              null comment 'DAO在上一个区块结束时资金池里的总金额',
    unchanged_token_amount           decimal(36, 18)                              null comment 'DAO在上一个区块结束时所有未兑换的代币数量',
    transfer_token                   decimal(36, 18)                              null comment 'transfer token',
    canvas_created_whitelist         int             default 0                    null comment '是否开通创建canvas白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通',
    canvas_created_blacklist         int             default 0                    null comment '是否开通创建canvas黑名单 0-未开通 1-开通',
    minter_works_whitelist           int             default 0                    null comment '是否开通铸造nft白名单 0-未开通 1-开通address 2- 开通ERC721 3-address和ERC721都开通',
    minter_works_blacklist           int             default 0                    null comment '是否开通铸造nft黑名单 0-未开通 1-开通',
    social_links                     text                                         null comment '社交链接 多个用逗号分隔',
    dao_symbol                       varchar(45)                                  null comment 'dao erc20 symbol',
    fresh_opensea                    int             default 0                    null comment '是否需要刷新opensea 0-不需要 1-需要',
    mint_cap                         int             default 0                    null comment '是否开启 address 高优白名单 0-未开启 1-已开启',
    erc721_mint_cap                  int             default 0                    null comment '是否开启 Erc721 高优白名单 0-未开启 1-已开启',
    global_mint_cap                  int             default 0                    null comment 'dao全局铸造上限',
    sync_dex                         int             default 0                    null comment '是否需要同步dex 0-不需要 1-需要',
    dao_reward                       decimal(36, 18)                              null comment 'DAO发放的所有ERC20数量',
    burn_amount                      decimal(36, 18)                              null comment 'DAOburn的erc20数量',
    liquidity_pool                   int             default 0                    null comment '是否开启流动性 0-未开启 1-已开启',
    erc20_name                       varchar(45)                                  null comment 'erc20 name 例：D4A Token for D4A@1',
    splitter_address                 varchar(45)                                  null comment 'splitter合约地址',
    royalty_fee_income               decimal(36, 18)                              null comment '版税二次交易收益',
    dao_version                      int             default 1                    null comment 'dao版本 1-1.8.5前的版本 2-1.8.5版本的 3-1.8.5之后的版本',
    royalty_token                    varchar(200)                                 null comment '代币分配策略',
    fixed_reserve_ratio              varchar(200)                                 null comment '一口价铸造收益分配策略',
    unfixed_reserve_ratio            varchar(200)                                 null comment '非一口价铸造收益分配策略',
    royalty_token_generation_method  int             default 1                    null comment '代币发放逻辑 0-线性生成 1-衰减曲线  对应rewardTemplateType',
    royalty_token_lottery_mode       int             default 0                    null comment '乐透模式 线性DRB释放ERC20 0-关闭 1-开启',
    canvas_price_fluctuation_method  int             default 0                    null comment 'canvas价格变化规律 0-指数增加 1-线性增长',
    fluctuation_method_factor        decimal(36, 6)                               null comment 'canvas价格增长系数 依赖于canvasPriceFluctuationMethod字段',
    royalty_token_generation_factory decimal(16, 6)                               null comment '奖励发放衰减的系数',
    create_time                      datetime        default CURRENT_TIMESTAMP    null comment '创建时间',
    basic_dao                        int             default 1                    null comment '是否为basic dao 1-proto dao 2- basic dao',
    dao_flow                         decimal(36, 18)                              null comment 'dao 交易流水',
    add_work                         int             default 1                    null comment '程序判断是否生成work的字段 添加work 0-需要添加 1-不需要添加',
    work_url_suffix                  varchar(20)                                  null comment '自动生成图片地址后缀',
    dao_work_url                     varchar(200)                                 null comment '自动生成图片的地址',
    height                           decimal(16, 6)                               null comment '自动生成图片的高度',
    color                            varchar(20)                                  null comment '自动生成图片颜色',
    work_hash                        varchar(50)                                  null comment '自动生成图片的workHash',
    exist_dao_id                     varchar(200)                                 null comment '用户选择关联的旧 Basic DAO v1.1',
    daily_mint_cap                   int             default 10000                null comment 'dao每天可以铸造的上限  最大值为10000 v1.1',
    need_mintable_work               int             default 1                    null comment 'PASS模式 合约设置的字段 是否需要创建1000张work 0-需要 1-不需要',
    global_dao_price                 decimal(16, 6)                               null comment 'dao全局铸造价格 Unified Price 可以为零',
    generate_work_set                int             default 0                    null comment '设置自动生成work的总数量 0-为未设置',
    dao_redeem_pool                  varchar(200)                                 null comment 'DAO Redeem池',
    is_thirdparty_token              int             default 0                    null comment '是否为外部ERC20 0-否 1-是',
    topup_mode                       int             default 0                    null comment '是否开启了TopUp模式 0-否 1-是',
    eth_royalty_token                varchar(200)                                 null comment 'ETH分配策略',
    is_ancestordao                   int             default 0                    null comment '是否为MainDAO 0-否 1-是',
    received_eth                     decimal(36, 18) default 0.000000000000000000 null comment '已领取的eth数量',
    unclaimed_eth                    decimal(36, 18) default 0.000000000000000000 null comment '未领取eth数量',
    infinite_mode                    int             default 0                    null comment '是否开启无限模式，开启时返回1，关闭时返回0',
    erc20_payment_mode               int             default 0                    null comment '是否开启Erc20支付模式，开启时为1，关闭时为0。',
    dao_start_block                  varchar(60)                                  null comment 'dao开始block¦',
    duration                         varchar(60)                                  null comment 'dao的每个mintableRound的持续时间',
    remaining_mint_window            int             default 0                    null comment '剩余mintWindow',
    current_round                    int             default 0                    null comment 'dao 当前周期数',
    last_active_round                int             default 0                    null comment '最后一个活跃周期的编号',
    erc20_token_decimals             int                                          null comment 'erc20支付模式下，decimals小数位数',
    is_together_dao                  int             default 0                    null comment '是否为聚合dao，0-否，1-是',
    together_dao_id                  int                                          null comment '聚合dao的daoID',
    token_holders                    int             default 0                    null comment 'token的holder数量',
    last_modify_round                int             default 1                    null comment '重新开始的mintWindow 默认0',
    subdao_asset_pool_balance        varchar(200)                                 null comment 'subDao AssetPool Balance 展示最新的balance余额',
    dao_restart_block                varchar(60)                                  null comment 'dao重启后的区块高度',
    treasury_erc20                   varchar(200)                                 null comment '国库 对应的erc20 token地址',
    eth_token_royalty                decimal(16, 6)  default 0.000000             null comment 'eth解锁dao token的wallet比例(未开启wec20)',
    token_eth_royalty                decimal(16, 6)  default 0.000000             null comment 'dao token解锁eth的wallet比例(开启erc20)',
    dao_nft_erc721                   varchar(200)                                 null comment '创建dao 生成的nft凭证721',
    grant_dao_nft_erc721             varchar(200)                                 null comment '创建dao，给sub dao 打款生成的erc721地址',
    grant_treasury_nft_erc721        varchar(200)                                 null comment '创建Main dao用于给国库打款赠送的erc721的地址',
    pay_currency_type                varchar(255)                                 null comment '支付货币类型',
    input_token_logo                 varchar(255)                                 null comment 'input token的logo地址',
    input_token_address              varchar(200)                                 null comment 'input token的address',
    input_token_decimals             int                                          null comment 'input token的decimals',
    canvas_created_whitelist_nft     int             default 0                    null comment '是否开通创建canvas下某nft白名单  0关闭 1-开启',
    minter_works_whitelist_nft       int             default 0                    null comment '是否开通erc721下某nft白名单  0关闭 1-开启',
    erc721_mint_cap_id               int             default 0                    null comment '是否开启erc721下的某个nft铸造白名单  0关闭 1-开启',
    constraint dao_number
        unique (dao_number)
)
    comment 'dao' charset = utf8mb4;

create table if not exists protodao_fork.dao_allocation_amount
(
    id               int auto_increment comment 'id'
        primary key,
    type             int                                null comment '0-daoToken分配 1-eth分配',
    allocation_type  int                                null comment '0-其他dao分配 1-当前dao分配 2-当前dao的redeem池分配',
    from_dao_id      varchar(200)                       null comment '分配的fromDao的projectId',
    to_dao_id        varchar(200)                       null comment '分配的toDaoId的projectId',
    token            varchar(200)                       null comment 'token地址 0x0代表eth分配',
    to_did           int                                null comment 'toDao的dao表的id',
    to_dao_name      varchar(200)                       null comment 'toDao的DAO名称',
    to_dao_number    int                                null comment 'toDao的dao编号',
    amount           decimal(36, 18)                    null comment '分配的数量',
    round_drb        int                                null comment '分配时的drb',
    transaction_hash varchar(200)                       null comment '交易hash',
    block_time       varchar(45)                        null comment '交易hash上链时间',
    is_del           int      default 0                 null comment '0-未删除 1-已删除',
    create_time      datetime default CURRENT_TIMESTAMP null comment '创建时间',
    update_time      datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间'
)
    comment 'dao分配数量表' charset = utf8mb4;

create table if not exists protodao_fork.dao_allocation_strategy
(
    id                 int auto_increment comment 'id'
        primary key,
    origin_project_id  varchar(200)                       null comment '分配的dao的原始projectid',
    type               int                                null comment '0-dao token 1-eth',
    dao_id             int                                null comment 'dao id',
    project_id         varchar(200)                       null comment 'dao的projectid',
    dao_name           varchar(200)                       null comment 'DAO名称',
    dao_number         int                                null comment 'dao编号',
    royalty_proportion decimal(16, 6)                     null comment '分配比例',
    transaction_hash   varchar(200)                       null comment '交易hash',
    block_time         varchar(45)                        null comment '交易hash上链时间',
    royalty_type       int      default 0                 null comment '分配类型 0-非当前dao 1-redeem Asset Pool 2-selfReward 3-不出块比例',
    is_del             int      default 0                 null comment '0-未删除 1-已删除',
    create_time        datetime default CURRENT_TIMESTAMP null comment '创建时间',
    update_time        datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间'
)
    comment 'dao分配策略表' charset = utf8mb4;

create table if not exists protodao_fork.dao_append_token_log
(
    id                  int auto_increment comment 'id'
        primary key,
    dao_id              int                                null comment 'dao id',
    project_id          varchar(200)                       null comment 'dao projectId',
    origin_total_supply decimal(36, 18)                    null comment '原始dao代币数量',
    new_total_supply    decimal(36, 18)                    null comment '最新代币数量',
    user_address        varchar(200)                       null comment '追加人地址，非第三方时为空',
    is_thirdparty       int      default 0                 null comment '是否为外部ERC20 0-否 1-是',
    transaction_hash    varchar(200)                       null comment '交易hash',
    block_time          varchar(45)                        null comment '交易hash上链时间',
    is_del              int      default 0                 null comment '0-未删除 1-已删除',
    create_time         datetime default CURRENT_TIMESTAMP null comment '创建时间',
    update_time         datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint UNIQUE_transaction_hash
        unique (transaction_hash, project_id)
)
    comment 'dao代币追加记录' charset = utf8mb4;

create table if not exists protodao_fork.dao_daily_statistics
(
    id                            int auto_increment comment 'id'
        primary key,
    dao_id                        int                                null comment 'dao的id',
    project_id                    varchar(200)                       null comment 'dao的projectid',
    asset_pool_token_total_amount decimal(36, 18)                    null comment '资金池内总金额',
    asset_pool_token_income       decimal(36, 18)                    null comment '资金池收入',
    asset_pool_token_cost         decimal(36, 18)                    null comment '资金池支出',
    asset_pool_token_variation    decimal(36, 18)                    null comment '资金池变化量',
    asset_pool_eth_total_amount   decimal(36, 18)                    null comment '资金池内总金额',
    asset_pool_eth_income         decimal(36, 18)                    null comment '资金池收入',
    asset_pool_eth_cost           decimal(36, 18)                    null comment '资金池支出',
    asset_pool_eth_variation      decimal(36, 18)                    null comment '资金池变化量',
    status                        int                                null comment '计算状态 0-未计算 1-计算中 2-计算完成',
    record_time                   bigint unsigned                    null comment '记录时间戳',
    modify_time                   datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint project_id_record_time
        unique (project_id, record_time)
)
    comment 'Dao资金池每日零点统计数据' collate = utf8mb4_unicode_ci;

create table if not exists protodao_fork.dao_drb_statistics
(
    id                    int auto_increment comment 'id'
        primary key,
    dao_id                int                                null comment 'Dao id',
    drb_number            int                                null comment 'Drb区块号',
    floor_price           decimal(16, 6)                     null comment 'DAO在该区块结束时所有canvas的最低价',
    seven_day_drb_vol     decimal(16, 6)                     null comment 'DAO在该区块和之前的六个区块（包括轮空的区块）铸造费用总和',
    dao_asset_pool        decimal(16, 6)                     null comment 'DAO在该区块结束时资金池里的总金额',
    dao_reward            decimal(16, 6)                     null comment 'DAO在该区块结束时所发放的所有ERC20数量',
    canvas                int                                null comment 'DAO在该区块结束时canvas的数量',
    owners                varchar(45)                        null comment 'DAO在该区块结束时所有NFT所在地址的私钥拥有者去重计数',
    nft                   varchar(45)                        null comment 'DAO在该区块结束时所有NFT的数量',
    dre                   varchar(45)                        null comment 'Mint Revenue+未来收益',
    drb_vol               decimal(16, 6)                     null comment '当前drb的铸造费用',
    status                int                                null comment '0-未计算 1-计算中 2-计算完成',
    works                 int                                null comment 'DAO在该区块结束时所有work的数量',
    mint_revenue          decimal(16, 6)                     null comment '当前dao铸造总金额',
    times                 int      default 0                 null comment '计算失败次数',
    mint_revenue_ex_tax   decimal(16, 6)                     null comment '当前dao铸造总金额 mintRevenue*30%或者35% Dao的铸造总收入*0.3',
    drb_vol_ex_tax        decimal(16, 6)                     null comment 'Dao在当前区块铸造费用总和 除税 Dao在当前区块铸造费用总和*0.3',
    create_time           datetime default CURRENT_TIMESTAMP null comment '创建时间',
    record_time           bigint unsigned                    null comment '归属时间 用于DaoDrbStatistics统计时用 如果统计时未完成的则更改时间到下一个自然日',
    asset_pool_token_cost decimal(36, 18)                    null comment 'DAO在该区块结束时总出块token减去不出块部分',
    asset_pool_eth_cost   decimal(36, 18)                    null comment 'DAO在该区块结束时总出块eth减去不出块部分',
    contribution          decimal(36, 18)                    null comment '当前drb的铸造nft的贡献度',
    erc20_amount          decimal(36, 18)                    null comment '当前drb铸造出块的ERC20总量',
    eth_amount            decimal(36, 18)                    null comment '当前drb铸造出块的ETH总量',
    constraint dao_drb_number
        unique (dao_id, drb_number)
)
    comment 'Dao在drb的统计信息' charset = utf8mb4;

create table if not exists protodao_fork.dao_strategy
(
    id               int auto_increment comment 'id'
        primary key,
    type             int                                null comment '0-create_canvas 1-mint_work',
    strategy_type    int                                null comment '1-白名单 2-erc721白名单 3-黑名单',
    origin_address   longtext                           null comment 'erc721地址或者黑白名单地址 黑白名单地址以逗号分隔',
    proof_id         int                                null comment '白名单地址时代表proof_store表id，erc721时代表daoId',
    transaction_hash varchar(200)                       null comment '交易hash',
    block_time       varchar(45)                        null comment '交易hash上链时间',
    dao_uri          varchar(400)                       null comment 'dao的uri地址',
    dao_id           int                                null comment 'dao的id',
    project_id       varchar(200)                       null comment 'dao的projectid',
    dao_number       int                                null comment 'dao的编号',
    is_del           int      default 0                 null comment '0-未删除 1-已删除',
    is_valid         int      default 1                 null comment '0-无效 1-有效',
    create_time      datetime default CURRENT_TIMESTAMP null comment '创建时间',
    update_time      datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint strategy_type
        unique (type, strategy_type, dao_id)
)
    comment 'dao黑白名单策略表' charset = utf8mb4;

create table if not exists protodao_fork.favorites
(
    id           int auto_increment comment 'id'
        primary key,
    user_address varchar(42)                        null comment '用户地址',
    create_time  datetime default CURRENT_TIMESTAMP null comment '创建时间',
    type         int                                null comment '收藏类型 0-DAO 1-Canvas 3-works',
    favorite_id  varchar(66)                        null comment '目标id',
    constraint user_address
        unique (user_address, type, favorite_id)
)
    comment '收藏表' charset = utf8mb4;

create table if not exists protodao_fork.incentive_plan
(
    id                   int auto_increment comment 'id'
        primary key,
    dao_id               int                                     not null comment '聚合dao id',
    project_id           varchar(200) collate utf8mb4_unicode_ci not null comment '聚合dao project id',
    plan_code            varchar(200) collate utf8mb4_unicode_ci not null comment '合约生成的planId',
    plan_number          int                                     not null comment '后端生成的plan Number序号',
    transaction_hash     varchar(66) collate utf8mb4_unicode_ci  null comment '记录的交易hash',
    current_round        int                                     not null comment '当前周期',
    plan_name            varchar(200) collate utf8mb4_unicode_ci null comment 'plan名称',
    plan_logo_url        varchar(300) collate utf8mb4_unicode_ci null comment 'logo url',
    incentive_type       int                                     not null comment '计算激励贡献度的类型 1-input token 2-output token',
    input_token_symbol   varchar(200) collate utf8mb4_unicode_ci not null comment 'input token symbol',
    output_token_symbol  varchar(200) collate utf8mb4_unicode_ci not null comment 'output token symbol',
    reward_type          int                                     null comment 'plan激励的token方式(input,output,custom)',
    reward_token         varchar(200)                            null comment 'plan激励的token地址(input token,output token,custom token)',
    reward_token_symbol  varchar(200)                            null comment 'payment token 的symbol',
    reward_token_decimal int                                     null comment 'payment token 的 decimal',
    start_date           date                                    not null comment '开始时间',
    start_block          varchar(66) collate utf8mb4_unicode_ci  not null comment '开始区块高度',
    amount_source        varchar(200) collate utf8mb4_unicode_ci not null comment '奖励来源 1-国库 2-钱包',
    incentive_amount     decimal(36, 18)                         not null comment '计划激励金额',
    remaining_token      decimal(36, 18)                         not null comment '剩余token数量',
    plan_block_window    int                                     not null comment 'plan的周期数量',
    duration             int                                     not null comment '每个周期的周期持续时间，单位hours',
    incentive_status     int                                     not null comment '计划状态  1-已创建未开始 2-已开始 3-已结束 4-已停机',
    plan_uri             varchar(400) collate utf8mb4_unicode_ci null comment 'plan的uri',
    create_by            varchar(66) collate utf8mb4_unicode_ci  not null comment 'plan create user address',
    create_time          datetime default CURRENT_TIMESTAMP      null comment '创建时间'
)
    comment '激励计划表' charset = utf8mb4;

create table if not exists protodao_fork.nft_reward_allocation
(
    id                 int auto_increment comment 'id'
        primary key,
    dao_id             int                                     not null comment '聚合dao id',
    project_id         varchar(200) collate utf8mb4_unicode_ci not null comment '聚合dao project id',
    work_id            int                                     not null comment 'nft的work id',
    erc721_address     varchar(200) collate utf8mb4_unicode_ci not null comment 'erc721 address',
    work_number        int                                     not null comment 'work number',
    plan_code          varchar(200) collate utf8mb4_unicode_ci null comment '合约生成的planId',
    plan_reward_amount decimal(36, 18)                         null comment '可以collect的数量',
    create_time        datetime default CURRENT_TIMESTAMP      null comment '创建时间'
)
    comment 'nft奖励记录表' charset = utf8mb4;

create table if not exists protodao_fork.shutdown_record
(
    id               int auto_increment comment 'id'
        primary key,
    type             int                                not null comment '0-平台停机 1-dao停机 2-canvas停机',
    is_paused        int      default 0                 null comment '是否停机 0-停机 1-启动',
    record_id        int                                null comment 'daoid或者canvasid',
    block_number     varchar(45)                        null comment '区块号',
    block_time       varchar(45)                        null comment '领取时间',
    transaction_hash varchar(120)                       null comment 'Work铸造时的交易hash，默认为空',
    drb_number       int                                null comment '铸造时所属drb区块',
    is_del           int      default 0                 null comment '0-未删除 1-已删除',
    is_valid         int      default 1                 not null comment ' 0-无效 1-有效',
    create_time      datetime default CURRENT_TIMESTAMP null comment '创建时间',
    shutdown_id      varchar(200)                       null comment 'projetId或者canvasId',
    constraint UNIQUE_transaction_hash
        unique (transaction_hash)
)
    comment '停机记录表' charset = utf8mb4;

create table if not exists protodao_fork.subscribe
(
    id               int auto_increment comment 'id'
        primary key,
    contract_address varchar(66)                        null comment '合约地址',
    topics           varchar(500)                       null comment '订阅的主题或者方法名',
    from_block       varchar(66)                        null comment '开始块高度',
    receive_address  varchar(66)                        null comment '接收回调地址',
    filter_id        varchar(200)                       null comment '订阅ID',
    create_time      datetime default CURRENT_TIMESTAMP null comment '创建时间´',
    trade_type       varchar(45)                        null,
    is_del           int      default 0                 null,
    status           int      default 0                 null,
    order_init       int                                null,
    interval_time    int      default 10                null comment '间隔时间 秒 默认10秒',
    constraint UNIQUE_contract_address_topics
        unique (contract_address, topics, trade_type),
    constraint UNIQUE_filter_id
        unique (filter_id)
)
    comment '订阅记录' charset = utf8mb4;

create table if not exists protodao_fork.subscribe_bak
(
    id               int      default 0                 not null comment 'id',
    contract_address varchar(66) charset utf8mb4        null comment '合约地址',
    topics           varchar(500) charset utf8mb4       null comment '订阅的主题或者方法名',
    from_block       varchar(66) charset utf8mb4        null comment '开始块高度',
    receive_address  varchar(66) charset utf8mb4        null comment '接收回调地址',
    filter_id        varchar(200) charset utf8mb4       null comment '订阅ID',
    create_time      datetime default CURRENT_TIMESTAMP null comment '创建时间',
    trade_type       varchar(45) charset utf8mb4        null,
    is_del           int      default 0                 null,
    status           int      default 0                 null,
    order_init       int                                null,
    interval_time    int      default 10                null comment '间隔时间 秒 默认10秒'
);

create table if not exists protodao_fork.token_received_record
(
    id                int auto_increment comment 'id'
        primary key,
    token_num         decimal(36, 18)                              null comment '领取token数量',
    receive_type      int                                          null comment '领取类型1-dao领取 2-canvas领取 3-transfer 4-mint领取 5-解锁erc20',
    receive_id        int                                          null comment 'daoid或者canvasid',
    receive_address   varchar(66)                                  null comment '领取人地址',
    project_id        varchar(200)                                 null comment 'dao的projectid',
    canvas_id         varchar(200)                                 null comment 'canvas的id',
    block_number      varchar(45)                                  null comment '区块号',
    block_time        varchar(45)                                  null comment '领取时间',
    transaction_hash  varchar(120)                                 null comment '交易hash',
    drb_number        int                                          null comment '属于某个drb',
    eth_amount        decimal(36, 18)                              null comment '兑换获得的eth数量',
    token_type        int             default 0                    null comment '交易类型0-collect 1-swap 2-transfer 3-unlock',
    from_address      varchar(200)                                 null comment 'from的地址',
    to_address        varchar(200)                                 null comment 'to的地址,发起交易的address',
    dao_number        int                                          null comment 'dao num',
    token_num_balance decimal(36, 18) default 0.000000000000000000 null comment 'transfer的token的剩余数量',
    sync_dex          int                                          null comment '是否同步dex,只有swap需要同步 0-未同步 1-已同步',
    create_time       datetime        default CURRENT_TIMESTAMP    null comment '创建时间',
    type              int             default 0                    null comment '0-dao token 1-eth',
    eth_num           decimal(36, 18) default 0.000000000000000000 null comment '领取eth数量',
    constraint UNIQUE_transaction_hash
        unique (transaction_hash, token_type, receive_type, receive_id, token_num, receive_address)
)
    comment '代币领取记录表' charset = utf8mb4;

create table if not exists protodao_fork.treasury_transaction
(
    id                      int auto_increment comment 'id'
        primary key,
    project_id              varchar(200)                       null comment 'daoID',
    transaction_hash        varchar(66)                        null comment '记录的交易hash',
    from_address            varchar(200)                       null comment 'from的地址',
    to_address              varchar(200)                       null comment 'to的地址',
    block_number            varchar(66)                        null comment '上链时间区块高度',
    transaction_type        int(1)                             null comment '交易类型 0-给sub node打款 1-给国库打款',
    generate_erc721_address varchar(200)                       null comment '赠送的NFT的721地址',
    generate_token_id       varchar(200)                       null comment '赠送的NFT的token id',
    amount                  decimal(36, 18)                    null comment '打款或收款的amount',
    is_use_treasury         int(1)                             null comment '是否通过国库打款',
    create_time             datetime default CURRENT_TIMESTAMP null comment '创建时间',
    sub_dao_project_id      varchar(200)                       null comment 'subDaoID'
)
    comment '国库交易表' collate = utf8mb4_unicode_ci;

create table if not exists protodao_fork.user
(
    id                     int auto_increment comment 'id'
        primary key,
    user_address           varchar(42)                        null comment '用户地址',
    user_name              varchar(500)                       null comment '用户昵称',
    user_introduction      text                               null comment '用户个人介绍',
    avatar_address         varchar(400)                       null comment '头像地址',
    opensea_link           varchar(400)                       null comment 'opensea链接',
    twitter_link           varchar(400)                       null comment 'Twitter链接',
    discord_link           varchar(400)                       null comment 'discordé“¾æŽ¥',
    first_login_time       timestamp                          null comment 'discord链接',
    sign_privacy_agreement int                                null comment '是否签署隐私协议0-未签署1-已签署',
    role                   tinyint  default 0                 null comment '用户权限0-无权限 1-OPERATION_ROLE 2-DEFAULT_ADMIN_ROLE 3-DAO_ROLE 4-PROJECT_ROLE',
    transaction_hash       varchar(66)                        null comment '交易Hash',
    is_contract            tinyint  default 0                 null comment '是否为合约地址 0-否 1-是',
    create_time            datetime default CURRENT_TIMESTAMP null comment '创建时间',
    constraint UNIQUE_user_address
        unique (user_address)
)
    comment '用户信息表' charset = utf8mb4;

create table if not exists protodao_fork.user_harvest_token
(
    id                    int auto_increment comment 'id'
        primary key,
    dao_id                int                                          not null comment 'dao id',
    canvas_id             int                                          not null comment 'canvas 表id 仅随机存一个canvasId，目前一个dao只有一条记录',
    total_token           decimal(36, 18)                              null comment '获得token的总数量',
    received_token        decimal(36, 18)                              null comment '已领取未使用的token数量',
    unclaimed_token       decimal(36, 18)                              null comment '未领取token数量',
    transfer_token        decimal(36, 18)                              null comment 'transfer token数量',
    swap_token            decimal(36, 18)                              null comment '已兑换token数量',
    swap_eth              decimal(36, 18)                              null comment '已兑换eth数量',
    user_address          varchar(128)                                 null comment '用户地址',
    last_transaction_hash varchar(66)                                  null comment '最后一次修改的hash',
    create_time           datetime        default CURRENT_TIMESTAMP    not null comment '创建时间',
    modify_time           datetime        default CURRENT_TIMESTAMP    not null on update CURRENT_TIMESTAMP comment '更新时间',
    received_eth          decimal(36, 18) default 0.000000000000000000 null comment '已领取未使用的eth数量',
    unclaimed_eth         decimal(36, 18) default 0.000000000000000000 null comment '未领取eth数量',
    constraint user_dao
        unique (user_address, dao_id)
)
    comment '用户mint获得的代币';

create table if not exists protodao_fork.user_topup_harvest
(
    id                 int auto_increment comment 'id'
        primary key,
    user_address       varchar(200)                       null comment '用户地址',
    dao_id             int                                null comment 'mainDao dao id',
    project_id         varchar(200)                       null comment 'mainDao projectId',
    subdao_erc20_token varchar(200)                       null comment 'erc20Token地址',
    erc20_amount       decimal(36, 18)                    null comment 'erc20余额',
    eth_amount         decimal(36, 18)                    null comment 'eth余额',
    round_drb          int                                null comment '分配时的drb',
    is_del             int      default 0                 null comment '0-未删除 1-已删除',
    create_time        datetime default CURRENT_TIMESTAMP null comment '创建时间',
    update_time        datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间'
)
    comment 'topup模式用户拥有eth和token的数量' charset = utf8mb4;

create table if not exists protodao_fork.white_list
(
    id              int auto_increment comment 'id'
        primary key,
    user_address    varchar(42)                        null comment '用户地址',
    origin_address  longtext                           null comment '原始地址',
    proof           longtext                           null comment 'proof信息',
    proof_root_hash varchar(200)                       null comment 'proof根hash',
    is_del          int      default 0                 null comment '0-未删除 1-已删除',
    is_valid        int      default 1                 null comment '0-无效 1-有效',
    create_time     datetime default CURRENT_TIMESTAMP null comment '创建时间',
    update_time     datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint user_address
        unique (user_address, proof_root_hash)
)
    comment '黑白名单地址表' charset = utf8mb4;

create table if not exists protodao_fork.work
(
    id                  int auto_increment comment 'id'
        primary key,
    work_number         int                                   null comment 'work编号',
    image_url           varchar(400)                          null comment '图片地址',
    work_description    text                                  null comment 'work描述',
    canvas_id           varchar(200)                          null comment 'canvas id',
    canvas_number       int                                   null comment 'canvas编号',
    dao_id              int                                   null comment 'dao的id',
    project_id          varchar(200)                          null comment 'dao的projectid',
    dao_number          int                                   null comment 'dao的编号',
    transaction_hash    varchar(66)                           null comment 'Work铸造时的交易hash，默认为空',
    creator_address     varchar(42)                           null comment '创建人地址-canvas得拥有者地址',
    minted_address      varchar(42)                           null comment '铸造者地址',
    minted_price        decimal(30, 6)                        null comment '铸造价格',
    block_time          varchar(45)                           null comment '铸造的区块时间',
    drb_number          int                                   null comment '铸造时所属drb区块',
    owner_address       varchar(66)                           null comment '当前owner地址',
    work_uri            varchar(400)                          null comment 'work的uri',
    work_status         int                                   null comment 'Work状态0-已创建1-已铸造2-已失效',
    work_hash           varchar(66)                           null comment 'work的hash值',
    block_number        varchar(45)                           null comment '铸造的区块号',
    create_time         datetime    default CURRENT_TIMESTAMP null comment '创建时间',
    favorite_amount     int                                   null comment '收藏数',
    is_del              int         default 0                 null comment '0-未删除 1-已删除',
    bg_color            varchar(45)                           null comment '图片背景色',
    height              decimal(16, 6)                        null comment '计算图片在260宽度时的高度',
    can_id              int                                   null comment '所属canvas表的id',
    create_sign_hash    varchar(200)                          null comment '创建时的签名hash',
    price_type          int         default 0                 null comment '价格类型 0-canvas_price 1-fixed_price',
    fixed_price         decimal(30, 6)                        null comment '一口价',
    generate            int         default 2                 null comment '是否自动生成 1-自动生成 2-上传的',
    social_links        text                                  null comment '社交链接',
    opensea_link        varchar(400)                          null comment 'opensea链接地址',
    twitter_link        varchar(400)                          null comment 'Twitter链接地址',
    discord_link        varchar(400)                          null comment 'Discord链接地址',
    topup_mode          int         default 0                 null comment '1.3 是否为topup模式铸造 0-非topup 1-topup下铸造topup的nft 2-topup下铸造非topup的nft',
    lock_status         int(1)      default 0                 null comment '锁定状态 0-未锁定 1-已锁定',
    lock_start_block    varchar(45) default '0'               null comment '锁定开始的区块高度',
    lock_duration_block varchar(45) default '0'               null comment '锁定的区块高度',
    mount_work_id       int                                   null comment '绑定的workId',
    constraint UNIQUE_transaction_hash
        unique (transaction_hash, dao_id, work_number)
)
    comment 'work作品' charset = utf8mb4;

create table if not exists protodao_fork.work_topup_harvest
(
    id                   int auto_increment comment 'id'
        primary key,
    work_id              int                                     null comment '新生成的nft',
    input_token_amount   decimal(36, 18)                         null comment 'input token的挂帐数量 ',
    output_token_amount  decimal(36, 18)                         null comment 'out put token的挂帐数量',
    dao_id               int                                     null comment '铸造nft所属的 主dao id',
    project_id           varchar(200)                            null comment '铸造nft所属的主dao的projectId',
    erc20_amount         decimal(36, 18)                         null comment '铸造nft的erc20余额',
    eth_amount           decimal(36, 18)                         null comment '铸造nft的eth余额',
    mount_erc721_address varchar(200) collate utf8mb4_unicode_ci null comment '消费dao的project对应的erc721address地址',
    mount_work_id        int                                     null comment '消费的work id',
    mount_work_number    int                                     null comment '消费的work编号',
    is_del               int      default 0                      null comment '0-未删除 1-已删除',
    create_time          datetime default CURRENT_TIMESTAMP      null comment '创建时间',
    update_time          datetime default CURRENT_TIMESTAMP      null on update CURRENT_TIMESTAMP comment '更新时间',
    constraint dao_id_mount_work_id
        unique (dao_id, mount_work_id)
)
    comment 'topup模式work下的eth和token的数量' charset = utf8mb4;

