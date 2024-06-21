package semios.dex.utils;

import com.baomidou.mybatisplus.generator.FastAutoGenerator;
import com.baomidou.mybatisplus.generator.engine.FreemarkerTemplateEngine;
import com.baomidou.mybatisplus.generator.function.ConverterFileName;

/**
 * @author fjtan
 */
public class Generator {

    public static void main(String[] args) {
        FastAutoGenerator.create("jdbc:mysql://127.0.0.1:3306/dao4art?useUnicode=true&characterEncoding=utf-8", "root",
                        "xb910716").globalConfig(builder -> {
                    builder.author("xiangbin") // 设置作者
                            // .enableSwagger() // 开启 swagger 模式
                            .fileOverride() // 覆盖已生成文件
                            .commentDate("").outputDir("/Users/xiangbin/temp"); // 指定输出目录
                }).packageConfig(builder -> {
                    builder.parent("semios.dex.boot") // 设置父包名
                            // .entity("model.entity.system"); // 设置mapperXml生成路径
                            .entity("model.entity"); // 设置mapperXml生成路径
                }).strategyConfig(builder -> {
                    // builder.addInclude("company", "user", "dict", "message", "log"); // 设置需要生成的表名
                    builder.addInclude("liquidity_transaction", "erc20_liquidity", "liquidity_price_record", "liquidity_daily_statistics", "user_liquidity_statistics"); // 设置需要生成的表名
                }).templateEngine(new FreemarkerTemplateEngine()) // 使用Freemarker引擎模板，默认的是Velocity引擎模板
                .execute();

    }

    static class MyConvertFileName implements ConverterFileName {

        private final String remove;

        public MyConvertFileName(String remove) {
            this.remove = remove;
        }

        @Override
        public String convert(String entityName) {
            return entityName.replaceAll("(?i)" + this.remove, "");
        }
    }
}
