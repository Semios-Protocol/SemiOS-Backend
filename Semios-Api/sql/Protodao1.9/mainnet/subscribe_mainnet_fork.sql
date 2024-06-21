# 数据全部清除，表结构从test中取

create table if not exists subscribe_pro_fork.block_height
(
    id          int auto_increment
        primary key,
    sub_id      int          null,
    from_block  varchar(45)  null,
    to_block    varchar(45)  null,
    filter_id   varchar(100) null,
    modify_time datetime     null
)
    AUTO_INCREMENT=11 comment '订阅区块高度表' charset = utf8mb4;

create table if not exists subscribe_pro_fork.sub_num_value
(
    id           int auto_increment comment 'id'
        primary key,
    address      varchar(64)                        null comment '合约地址',
    net_work     varchar(64)                        null comment 'netWork',
    topic        varchar(500)                       null comment '订阅的方法',
    block_height varchar(32)                        null comment '区块高度',
    value        varchar(128)                       null comment '区块上的值',
    update_time  datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    filter_id    int                                null
)
    AUTO_INCREMENT=11 comment '数值类型订阅最新值' charset = utf8mb4 ;

create table if not exists subscribe_pro_fork.subscriber
(
    id               int unsigned auto_increment
        primary key,
    network          varchar(45)                        not null comment '网络',
    from_block       varchar(45)                        null comment '开始块高度 为空则从当前块开始',
    address          varchar(45)                        null comment '监听地址',
    topics           varchar(500)                       null comment '监听主题',
    interval_time    int                                null comment '间隔时间 秒s 目前只有10和60两个类型和对应的定时任务',
    notice_url       varchar(300)                       null comment '通知地址',
    notice_err_times int      default 0                 null comment '通知异常次数超过10次则停止通知',
    sub_status       int      default 0                 null comment '监听状态 0-关闭 1-开启',
    is_del           int      default 0                 null comment '删除标识 0-未删除 1-已删除',
    create_time      datetime default CURRENT_TIMESTAMP not null,
    update_time      datetime default CURRENT_TIMESTAMP null on update CURRENT_TIMESTAMP comment '更新时间',
    notice_type      int      default 0                 null comment '通知类型',
    app_name         varchar(45)                        null comment '应用名称'
)
    AUTO_INCREMENT=11 comment '订阅记录表' charset = utf8mb4;

create table if not exists subscribe_pro_fork.transaction
(
    id                int auto_increment
        primary key,
    address           varchar(45)                        null,
    block_hash        varchar(66)                        null,
    block_number      varchar(45)                        null,
    block_int_num     int                                null comment 'blockNumber10进制',
    data              text                               null,
    log_index         varchar(45)                        null,
    removed           varchar(45)                        null,
    topics            varchar(1000)                      null,
    transaction_hash  varchar(66)                        null,
    transaction_index varchar(45)                        null,
    sub_id            int                                null,
    notice_times      int      default 0                 null comment '通知次数',
    notice_status     int      default 0                 null comment '通知状态 0-未成功 1-已成功',
    create_time       datetime default CURRENT_TIMESTAMP null comment '创建时间',
    block_timestamp   varchar(45)                        null comment 'bolck',
    app_name          varchar(45)                        null comment 'appname'
)
    AUTO_INCREMENT=11 comment 'transaction记录表' charset = utf8mb4;

