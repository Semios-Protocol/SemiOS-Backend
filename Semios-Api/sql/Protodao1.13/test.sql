# maker信息统计，需要添加表格
# 创建maker_info表,统计每日的maker信息
create table `maker_info_statistics`
(
    id                   int auto_increment comment 'id'
        primary key,
    dao_id               int                                null comment 'seed node id',
    project_id           varchar(200)                       null comment 'seed node project id',

    maker_address        int(20)                            null comment 'maker人数',
    input_token_balance  decimal(36, 18)                     null comment '未花费input token余额',
    output_token_balance decimal(36, 18)                       null comment '锁定的output Token余额',

    record_time          date                           null comment '记录时间',
    create_time          datetime default CURRENT_TIMESTAMP null comment '创建时间',
    UNIQUE INDEX idx_record_time_project_id (record_time, project_id)
)comment 'seed node maker统计信息' charset = utf8mb4;
