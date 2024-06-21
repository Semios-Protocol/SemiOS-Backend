-- nft 激励的symbol和发方奖励的token(input,out,custom)
alter table protodao.incentive_plan add `reward_type` int(11) DEFAULT NULL COMMENT 'plan激励的token方式(input,output,custom)' after output_token_symbol;
alter table protodao.incentive_plan add `reward_token` varchar(200) DEFAULT NULL COMMENT 'plan激励的token地址(input token,output token,custom token)' after reward_type;
alter table protodao.incentive_plan add `reward_token_symbol` varchar(200) DEFAULT NULL COMMENT 'payment token 的symbol' after reward_token;
alter table protodao.incentive_plan add `reward_token_decimal` int(11) DEFAULT NULL COMMENT 'payment token 的 decimal' after reward_token_symbol;


-- 之前创建的plan，添加reward_type,reward_token,reward_token_symbol,reward_token_decimal字段

