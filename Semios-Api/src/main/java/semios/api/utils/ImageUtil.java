package semios.api.utils;

import cn.hutool.core.io.FileTypeUtil;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.entity.ContentType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.security.MessageDigest;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * 图片处理工具类
 *
 * @description: 图片处理工具类
 * @author: xiangbin
 * @create: 2022-09-01 15:25
 **/
@Slf4j
public class ImageUtil {

    public static HeightAndBgColor getImageRgb(MultipartFile multipartFile, String imageHash) {
        // 获取图像的RGB值；
        BufferedImage bufferedImage = null;
        int r, g, b; // 分别用来存放获取的RGB值
        int height = 0, width = 0;
        int id = 0;
        // 读取要操作的图片，这里的图片路径请改成自己要处理的图片
        String fileName = multipartFile.getOriginalFilename();
        String fileSuffix = fileName.substring(fileName.lastIndexOf("."));
        String path = "/home/ubuntu/dao4art/api/logs/" + imageHash + fileSuffix;
        File file = getFileByMultipartFile(multipartFile, path);
        try {
            bufferedImage = ImageIO.read(file);
            // 获取图片的宽和高；
            if (bufferedImage == null) {
                log.error("[getImageRgb] bufferedImage is null");
                return null;
            }
            height = bufferedImage.getHeight();
            width = bufferedImage.getWidth();
            log.info("height   = " + height + ", width   =" + width);
            // 采用行优先遍历，先遍历宽

            int x = width / 3;
            int y = height / 3;
            Color color = new Color(bufferedImage.getRGB(x, y));
            r = color.getRed();
            g = color.getGreen();
            b = color.getBlue();
            log.info("此时的id为： " + id + "  R =  " + r + ",  G = " + g + ", B =" + b);

            ImageUtil imageUtil = new ImageUtil();
            HeightAndBgColor heightAndBgColor = imageUtil.new HeightAndBgColor();
            heightAndBgColor.setBgColor(convertRGBToHex(r, g, b));
            // ((260 / width ) * heigth
            heightAndBgColor
                    .setHeight(new BigDecimal("258").divide(new BigDecimal(String.valueOf(width)), 4, RoundingMode.FLOOR)
                            .multiply(new BigDecimal(String.valueOf(height))).setScale(0, RoundingMode.FLOOR).doubleValue());
            return heightAndBgColor;
        } catch (IOException e) {
            log.error("[getImageRgb]异常 e:", e);
            return null;
        } finally {
            if (bufferedImage != null) {
                if (!path.contains("..") || !path.contains("\\") || path.startsWith("/home/ubuntu/")) {
                    bufferedImage.flush();
                }
            }
            if (file.exists()) {
                if (!path.contains("..") || !path.contains("\\") || path.startsWith("/home/ubuntu/")) {
                    file.delete();
                }
            }
        }
    }

    public static HeightAndBgColor getImageRgb(File file) {
        // 获取图像的RGB值；
        BufferedImage bufferedImage = null;
        int r, g, b; // 分别用来存放获取的RGB值
        int height = 0, width = 0;
        int id = 0;
        try {
            bufferedImage = ImageIO.read(file);
            // 获取图片的宽和高；
            if (bufferedImage == null) {
                log.error("[getImageRgb] bufferedImage is null");
                return null;
            }
            height = bufferedImage.getHeight();
            width = bufferedImage.getWidth();
            log.info("height   = " + height + ", width   =" + width);
            // 采用行优先遍历，先遍历宽

            int x = width / 3;
            int y = height / 3;
            Color color = new Color(bufferedImage.getRGB(x, y));
            r = color.getRed();
            g = color.getGreen();
            b = color.getBlue();
            log.info("此时的id为： " + id + "  R =  " + r + ",  G = " + g + ", B =" + b);

            ImageUtil imageUtil = new ImageUtil();
            HeightAndBgColor heightAndBgColor = imageUtil.new HeightAndBgColor();
            heightAndBgColor.setBgColor(convertRGBToHex(r, g, b));
            // ((260 / width ) * heigth
            heightAndBgColor
                    .setHeight(new BigDecimal("258").divide(new BigDecimal(String.valueOf(width)), 4, RoundingMode.FLOOR)
                            .multiply(new BigDecimal(String.valueOf(height))).setScale(0, RoundingMode.FLOOR).doubleValue());
            return heightAndBgColor;
        } catch (IOException e) {
            log.error("[getImageRgb]异常 e:", e);
            return null;
        } finally {
            if (bufferedImage != null) {
                bufferedImage.flush();
            }
        }
    }

    /**
     * 获取上传文件的md5
     *
     * @param file
     * @return
     * @throws IOException
     */
    public static String getMd5(MultipartFile file) {
        try {
            // 获取文件的byte信息
            byte[] uploadBytes = file.getBytes();
            // 拿到一个MD5转换器
            MessageDigest md5 = MessageDigest.getInstance("MD5");
            byte[] digest = md5.digest(uploadBytes);
            // 转换为16进制
            return new BigInteger(1, digest).toString(16);
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return null;
    }

    public static String getMD5(File file) {
        FileInputStream fileInputStream = null;
        try {
            MessageDigest MD5 = MessageDigest.getInstance("MD5");
            String path = file.getPath();
            if (path.contains("..") || path.contains("\\") || !path.startsWith("/home/ubuntu/")) {
                log.error("[getMD5] path is invalid:{}", path);
                throw new IllegalArgumentException("Invalid filename");
            }

            fileInputStream = new FileInputStream(file);
            byte[] buffer = new byte[8192];
            int length;
            while ((length = fileInputStream.read(buffer)) != -1) {
                MD5.update(buffer, 0, length);
            }
            return new String(Hex.encodeHex(MD5.digest()));
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            try {
                if (fileInputStream != null) {
                    fileInputStream.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public static boolean isImage(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            return false;
        }
        String imageType = "image";
        Set<String> fileType = new HashSet<String>();
        fileType.add("jpg");
        fileType.add("png");
        fileType.add("gif");
        String type;
        try {
            type = FileTypeUtil.getType(file.getInputStream());
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
        String contentType = file.getContentType();
        // 判断是否在限制的文件类型集合中，如果为null，直接返回false
        if (!Optional.ofNullable(type).map(fileType::contains).orElse(false)) {
            return false;
        }
        // 判断content type是不是image
        return contentType.contains(imageType);
    }

    // ====================private=========//

    private static byte[] getContent(String filePath) throws IOException {
        File file = new File(filePath);
        long fileSize = file.length();
        if (fileSize > Integer.MAX_VALUE) {
            System.out.println("file too big...");
            return null;
        }
        FileInputStream fi = new FileInputStream(file);
        byte[] buffer = new byte[(int) fileSize];
        int offset = 0;
        int numRead = 0;
        while (offset < buffer.length && (numRead = fi.read(buffer, offset, buffer.length - offset)) >= 0) {
            offset += numRead;
        }
        // 确保所有数据均被读取
        if (offset != buffer.length) {
            throw new IOException("Could not completely read file " + file.getName());
        }
        fi.close();
        return buffer;
    }

    // **将rgb色彩值转成16进制代码**
    private static String convertRGBToHex(int r, int g, int b) {
        String rFString, rSString, gFString, gSString, bFString, bSString, result;
        int red, green, blue;
        int rred, rgreen, rblue;
        red = r / 16;
        rred = r % 16;
        if (red == 10)
            rFString = "A";
        else if (red == 11)
            rFString = "B";
        else if (red == 12)
            rFString = "C";
        else if (red == 13)
            rFString = "D";
        else if (red == 14)
            rFString = "E";
        else if (red == 15)
            rFString = "F";
        else
            rFString = String.valueOf(red);

        if (rred == 10)
            rSString = "A";
        else if (rred == 11)
            rSString = "B";
        else if (rred == 12)
            rSString = "C";
        else if (rred == 13)
            rSString = "D";
        else if (rred == 14)
            rSString = "E";
        else if (rred == 15)
            rSString = "F";
        else
            rSString = String.valueOf(rred);

        rFString = rFString + rSString;

        green = g / 16;
        rgreen = g % 16;

        if (green == 10)
            gFString = "A";
        else if (green == 11)
            gFString = "B";
        else if (green == 12)
            gFString = "C";
        else if (green == 13)
            gFString = "D";
        else if (green == 14)
            gFString = "E";
        else if (green == 15)
            gFString = "F";
        else
            gFString = String.valueOf(green);

        if (rgreen == 10)
            gSString = "A";
        else if (rgreen == 11)
            gSString = "B";
        else if (rgreen == 12)
            gSString = "C";
        else if (rgreen == 13)
            gSString = "D";
        else if (rgreen == 14)
            gSString = "E";
        else if (rgreen == 15)
            gSString = "F";
        else
            gSString = String.valueOf(rgreen);

        gFString = gFString + gSString;

        blue = b / 16;
        rblue = b % 16;

        if (blue == 10)
            bFString = "A";
        else if (blue == 11)
            bFString = "B";
        else if (blue == 12)
            bFString = "C";
        else if (blue == 13)
            bFString = "D";
        else if (blue == 14)
            bFString = "E";
        else if (blue == 15)
            bFString = "F";
        else
            bFString = String.valueOf(blue);

        if (rblue == 10)
            bSString = "A";
        else if (rblue == 11)
            bSString = "B";
        else if (rblue == 12)
            bSString = "C";
        else if (rblue == 13)
            bSString = "D";
        else if (rblue == 14)
            bSString = "E";
        else if (rblue == 15)
            bSString = "F";
        else
            bSString = String.valueOf(rblue);
        bFString = bFString + bSString;
        result = "#" + rFString + gFString + bFString;
        return result;

    }

    private static File getFileByMultipartFile(MultipartFile multipartFile, String path) {
        File file = new File(path);
        try {
            if (!path.startsWith("/home/ubuntu/dao4art/api/logs/")) {
                throw new IllegalArgumentException("Invalid filename");
            }
            FileUtils.copyInputStreamToFile(multipartFile.getInputStream(), file);
        } catch (Exception e) {
            log.error("[getFileByMultipartFile] error", e);
            e.printStackTrace();
        }
        return file;
    }

    public static String imageUrlResized(String imageUrl) {
        if (StringUtils.isNotBlank(imageUrl)) {
            if (imageUrl.endsWith(".gif")) {
                return imageUrl;
            }
            return imageUrl.replace("work", "resized-work");
        }
        return "";
    }

    /**
     * https://www.5axxw.com/questions/simple/3ifa0o
     */
    public static void imageAddText(String workImageDefaultUrl, String fileDir, String daoName, String workName, String formateName) {
        try {
            // 加载图片
            BufferedImage image = ImageIO.read(new File(workImageDefaultUrl));
            // 创建画布
            Graphics2D graphics = image.createGraphics();
            // 设置抗锯齿
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            // 设置字体和颜色
            Font daoFont = new Font("Arial", Font.BOLD, 32);
            Color daoColor = Color.WHITE;
            graphics.setFont(daoFont);
            graphics.setColor(daoColor);

            // 在画布上添加文字
            daoName = daoName.trim();
            int x = 60;
            int y = 120;
            int lineHeight = 40;
            int numberOfLines = 0;

            if (daoName.length() > 45) {
                daoName = daoName.substring(0, 45);
            }

            for (int i = 0; i < 4; i++) {
                int j = Math.min(((i + 1) * 12), daoName.length());
                if (i * 12 >= daoName.length()) break;
                graphics.drawString(daoName.substring(i * 12, j).toUpperCase(), x, y);
                y += lineHeight;
                numberOfLines++;
            }

            // 绘制 workName
            if (StringUtils.isNotBlank(workName)) {
                Font workFont = new Font("Arial", Font.PLAIN, 30);
                Color workColor = Color.WHITE;
                graphics.setFont(workFont);
                graphics.setColor(workColor);
                // 动态调整 workName 的 y 坐标
                graphics.drawString(workName, x, y + lineHeight); // 在最后一行文字下方绘制
            }

            // 保存图片
            //ImageIO.write(image, "png", new File("/Users/zhyyao/java/proToDao/protodao-api/src/main/resources/image/work_default-1.png"));
            ImageIO.write(image, formateName, new File(fileDir));
        } catch (Exception e) {
            log.error("[imageAddText] daoName:{} e:{}", daoName, e);
        }
    }

    public static void imageAddTextNft(String workImageDefaultUrl, String fileDir, String daoName, String formateName) {
        try {
            // 加载图片
            BufferedImage image = ImageIO.read(new File(workImageDefaultUrl));
            // 创建画布
            Graphics2D graphics = image.createGraphics();
            // 设置抗锯齿
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            // 设置字体和颜色
            Font daoFont = new Font("Arial", Font.BOLD, 80);
            Color daoColor = Color.WHITE;
            graphics.setFont(daoFont);
            graphics.setColor(daoColor);

            // 在画布上添加文字
            daoName = daoName.trim();
            int lineHeight = 80; // 设置行间距
            int x = 60;
            int y = 200;

            if (daoName.length() > 45) {
                daoName = daoName.substring(0, 45);
            }

            for (int i = 0; i < daoName.length(); i += 12) {
                int end = Math.min(i + 12, daoName.length());
                String line = daoName.substring(i, end).toUpperCase();
                graphics.drawString(line, x, y);
                y += lineHeight;
            }

            // 保存图片
            //ImageIO.write(image, "png", new File("/Users/zhyyao/java/proToDao/protodao-api/src/main/resources/image/nft_default-1.png"));
            ImageIO.write(image, formateName, new File(fileDir));
        } catch (Exception e) {
            log.error("[imageAddText] daoName:{} e:{}", daoName, e);
        }
    }

    public static void main(String[] args) {
        // ImageUtil.getImageRgb(null);
        // System.out.println(ImageUtil.convertRGBToHex(219,180,61));
        // System.out.println(ImageUtil.getMd5(null));
        // String fileName = "u=1090450172,838097269&fm=253&fmt=auto&app=120&f=JPEG.jpg";
        // System.out.println(fileName.substring(fileName.lastIndexOf(".")));
        // int heigth = 0, width = 0;
        // heigth = 173;
        // width = 260;
        // System.out.println(260/width);
        // System.out.println(new BigDecimal("260").divide(new BigDecimal(String.valueOf(width)), 4,
        // RoundingMode.HALF_UP).multiply(new BigDecimal(String.valueOf(heigth))).setScale(0,
        // RoundingMode.HALF_UP).doubleValue());

        // String imageUrl =
        // "https://test-dao4art-resized.s3.ap-southeast-1.amazonaws.com/work/09bef6265f75fa5c37d84e59be3b0e6b.jpeg";
        // System.out.println(ImageUtil.imageUrlResized(imageUrl));

//        Double width = 416.59;
//        Double height = 698.94;
//        Double ddd = new BigDecimal("258").divide(new BigDecimal(String.valueOf(width)), 4, RoundingMode.HALF_UP)
//                .multiply(new BigDecimal(String.valueOf(height))).setScale(0, RoundingMode.FLOOR).doubleValue();
//        System.out.println(ddd);

        imageAddText("src/main/resources/image/work_default.png", "", "yeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyee", "123asasdadas", "png");
        imageAddTextNft("src/main/resources/image/nft_default.png", "", "yeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyeeyee", "png");

        System.out.println(" 123 234 ".trim().length());
    }

    private MultipartFile getMultipartFile(File file) {
        FileInputStream fileInputStream = null;
        MultipartFile multipartFile = null;
        try {
            fileInputStream = new FileInputStream(file);
            multipartFile = new MockMultipartFile(file.getName(), file.getName(),
                    ContentType.APPLICATION_OCTET_STREAM.toString(), fileInputStream);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return multipartFile;
    }

    @Data
    public class HeightAndBgColor {

        private String bgColor;

        private Double height;

    }
}
