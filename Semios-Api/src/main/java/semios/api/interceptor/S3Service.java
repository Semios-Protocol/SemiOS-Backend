package semios.api.interceptor;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;
import semios.api.model.dto.common.BucketObjectRepresentaion;
import semios.api.utils.JacksonUtil;

import java.io.*;
import java.util.List;

/**
 * @description: S3Service
 * @author: xiangbin
 * @create: 2022-08-16 17:08
 **/
@Component
@Slf4j
public class S3Service {

    @Autowired
    private AmazonS3 amazonS3Client;

    public void createS3Bucket(String bucketName) {
        if (amazonS3Client.doesBucketExist(bucketName)) {
            log.info("Bucket name already in use. Try another name.");
            return;
        }
        amazonS3Client.createBucket(bucketName);
    }

    public List<Bucket> listBuckets() {
        return amazonS3Client.listBuckets();
    }

    public void deleteBucket(String bucketName) {
        try {
            amazonS3Client.deleteBucket(bucketName);
        } catch (AmazonServiceException e) {
            log.error(e.getErrorMessage());
            return;
        }
    }

    /**
     * Put an object in a bucket:
     *
     * @param bucketName
     * @param representation
     * @throws IOException
     */
    public void putObject(String bucketName, BucketObjectRepresentaion representation) throws Exception {

        String objectName = representation.getObjectName();
        String objectValue = representation.getText();

        File file = new File("." + File.separator + objectName);
        FileWriter fileWriter = new FileWriter(file, false);
        PrintWriter printWriter = new PrintWriter(fileWriter);
        printWriter.println(objectValue);
        printWriter.flush();
        printWriter.close();

        try {
            PutObjectRequest putObjectRequest =
                    new PutObjectRequest(bucketName, objectName, file).withCannedAcl(CannedAccessControlList.PublicRead);
            ObjectMetadata objectMetadata = new ObjectMetadata();
            objectMetadata.setContentType("application/json");
            putObjectRequest.setMetadata(objectMetadata);
            PutObjectResult putObjectResult = amazonS3Client.putObject(putObjectRequest);
            log.info("putObject result:{}", JacksonUtil.obj2json(putObjectResult));
        } catch (Exception e) {
            log.error("Some error has ocurred e", e);
            throw new RuntimeException("上传文件异常");
        }
    }

    /**
     * Put an object in a bucket:
     *
     * @param bucketName
     * @param multipartFile
     * @throws IOException
     */
    public void putImage(String bucketName, MultipartFile multipartFile, String imageName) throws IOException {

        String objectName = multipartFile.getOriginalFilename();
        OutputStream out = null;
        if (StringUtils.isNotBlank(imageName)) {
            objectName = imageName + objectName.substring(objectName.lastIndexOf("."));
        }
        File file = new File("." + File.separator + objectName);

        try {
            out = new FileOutputStream(file);
            byte[] ss = multipartFile.getBytes();
            for (int i = 0; i < ss.length; i++) {
                out.write(ss[i]);
            }

            PutObjectRequest putObjectRequest =
                    new PutObjectRequest(bucketName, objectName, file).withCannedAcl(CannedAccessControlList.PublicRead);
            ObjectMetadata objectMetadata = new ObjectMetadata();
            objectMetadata.setContentType("image/jpeg");
            putObjectRequest.setMetadata(objectMetadata);
            PutObjectResult putObjectResult = amazonS3Client.putObject(putObjectRequest);
            log.info("putImage result:{}", JacksonUtil.obj2json(putObjectResult));

        } catch (Exception e) {
            log.error("Some error has ocurred e", e);
        } finally {
            if (out != null) {
                out.flush();
                out.close();
            }
            if (file.exists()) {
                file.delete();
            }
        }
    }

    public void putImage(String bucketName, File file, String imageName, boolean deleteFile) throws IOException {
        try {
            PutObjectRequest putObjectRequest =
                    new PutObjectRequest(bucketName, imageName, file).withCannedAcl(CannedAccessControlList.PublicRead);
            ObjectMetadata objectMetadata = new ObjectMetadata();
            objectMetadata.setContentType("image/jpeg");
            putObjectRequest.setMetadata(objectMetadata);
            PutObjectResult putObjectResult = amazonS3Client.putObject(putObjectRequest);
            log.info("putImage result:{}", JacksonUtil.obj2json(putObjectResult));

        } catch (Exception e) {
            log.error("Some error has ocurred e", e);
        } finally {
            if (deleteFile && file.exists()) {
                file.delete();
            }
        }
    }

    /**
     * List all objects' names:
     *
     * @param bucketName
     * @return
     */
    public List<S3ObjectSummary> listObjects(String bucketName) {
        ObjectListing objectListing = amazonS3Client.listObjects(bucketName);
        return objectListing.getObjectSummaries();
    }

    /**
     * Make a download of an object:
     *
     * @param bucketName
     * @param objectName
     */
    public void downloadObject(String bucketName, String objectName) {
        S3Object s3object = amazonS3Client.getObject(bucketName, objectName);
        S3ObjectInputStream inputStream = s3object.getObjectContent();
        try {
            FileUtils.copyInputStreamToFile(inputStream, new File("." + File.separator + objectName));
        } catch (IOException e) {
            log.error(e.getMessage());
        }
    }

    /**
     * Delete an object:
     *
     * @param bucketName
     * @param objectName
     */
    public void deleteObject(String bucketName, String objectName) {
        amazonS3Client.deleteObject(bucketName, objectName);
    }

    /**
     * Delete multiple objects:
     *
     * @param bucketName
     * @param objects
     */
    public void deleteMultipleObjects(String bucketName, List<String> objects) {
        DeleteObjectsRequest delObjectsRequests =
                new DeleteObjectsRequest(bucketName).withKeys(objects.toArray(new String[0]));
        amazonS3Client.deleteObjects(delObjectsRequests);
    }

    /**
     * Moving an object between two buckets:
     *
     * @param bucketSourceName
     * @param objectName
     * @param bucketTargetName
     */
    public void moveObject(String bucketSourceName, String objectName, String bucketTargetName) {
        amazonS3Client.copyObject(bucketSourceName, objectName, bucketTargetName, objectName);
    }

}
