# 1.9版本事件调整修改


# CreateProjectParamEmitted 创建dao主事件
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x83cef7f63e09a3d2bb41629db1dee4c830cd02a8aa44c5e702e0ce482cad4351%';
update `subscribe` set topics='0xde8640b504fda36af7e64a1eea3efa660f74707b37cbf53badd85ce7f739a49f',filter_id = null,status=0 where topics like '%0x83cef7f63e09a3d2bb41629db1dee4c830cd02a8aa44c5e702e0ce482cad4351%';


# RatioSet 比例监听 RatioForFundingSet
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d%';
update `subscribe` set topics='0xdad95cbf6cfc944fb83b420714234dcd2d6b616e9ef36ec544b7354a099261f3',filter_id = null,status=0 where topics like '%0x8d9e310fb45c09f4acd92f08fb5cc227c5566cdfc80425d5a79069ad7e8e449d%';

# TopUpErc20Splitted 用户将erc20转换为eth 改为 TopUpOutputSplitted
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0xd12c877b4de5fd85038ab99742f3f9d677c4bae61298714c4bcbea2836c47d59%';
update `subscribe` set topics='0x0538d6798823484e47b6060a0dc3fa8975021e3c3ff05879f2f7a3647c0d2c61',filter_id = null,status=0 where topics like '%0xd12c877b4de5fd85038ab99742f3f9d677c4bae61298714c4bcbea2836c47d59%';


# TopUpEthSplitted  未监听
# DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x2dea5dcbb2d513ebaafa83b2ccad3ed30b0218b066d2eb1865f4776555cb05d7%';
# update `subscribe` set topics='0x0ec9a38c2664069c04b0c095474124c7e24c1d329d57141d955284dcfe02a0b7',filter_id = null,status=0 where topics like '%0x2dea5dcbb2d513ebaafa83b2ccad3ed30b0218b066d2eb1865f4776555cb05d7%';


# TopUpEthSplitRatioSet --> TopUpInputSplitRatioSet
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x26b15987e8b5c3f3eade30d4775bf68b51d00042b41b48861ca199489881e093%';
update `subscribe` set topics='0xc2d8f35cb6ec413664059e89c8d5dd8648d8384f9ad3f68932d6a61c4d7e528a',filter_id = null,status=0 where topics like '%0x26b15987e8b5c3f3eade30d4775bf68b51d00042b41b48861ca199489881e093%';

# TopUpErc20SplitRatioSet --> TopUpOutputSplitRatioSet
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x01db1017e94e3dd5318b6141da3775021fafe895c184ce30b736ab0a06b284ba%';
update `subscribe` set topics='0xa2b89e73fb67bcd7a7b6a705962a90fbc23ae1d1bd55b39b6e9546f3f46d90cb',filter_id = null,status=0 where topics like '%0x01db1017e94e3dd5318b6141da3775021fafe895c184ce30b736ab0a06b284ba%';

# D4AExchangeERC20ToETH --> D4AExchangeOutputToInput
DELETE sub , blo FROM subscribe_pro.subscriber AS sub, subscribe_pro.block_height AS blo WHERE sub.id = blo.sub_id AND sub.topics like '%0x513aa96d635def8ca996e250ecfb2839b372eae83235befdffb6eebe15cb9dcf%';
update `subscribe` set topics='0x674751277df8115e140ad56fbe245e13972f3eb35226d0b833c4053b37e46aca',filter_id = null,status=0 where topics like '%0x513aa96d635def8ca996e250ecfb2839b372eae83235befdffb6eebe15cb9dcf%';

