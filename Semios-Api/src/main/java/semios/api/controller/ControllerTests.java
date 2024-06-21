package semios.api.controller;

import com.amazonaws.services.s3.model.Bucket;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import semios.api.interceptor.S3Service;
import semios.api.model.dto.common.BucketObjectRepresentaion;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 测试接口
 *
 * @author xiangbin
 * @ignore 忽略
 */
@RestController
@RequestMapping(value = "/buckets/")
@RequiredArgsConstructor
public class ControllerTests {

    @Autowired
    private S3Service s3Service;

    /**
     * createBucket
     *
     * @ignore 忽略
     */
    @PostMapping(value = "/{bucketName}")
    public void createBucket(@PathVariable String bucketName) {
        s3Service.createS3Bucket(bucketName);
    }

    /**
     * listBuckets
     *
     * @ignore 忽略
     */
    @GetMapping(value = "/listBuckets")
    public List<String> listBuckets() {
        List<Bucket> buckets = s3Service.listBuckets();
        List<String> names = buckets.stream().map(Bucket::getName).collect(Collectors.toList());
        return names;
    }

    /**
     * deleteBucket
     *
     * @ignore 忽略
     */
    @DeleteMapping(value = "/{bucketName}")
    public void deleteBucket(@PathVariable String bucketName) {
        s3Service.deleteBucket(bucketName);
    }

    /**
     * createObject
     */
    @PostMapping(value = "/{bucketName}/objects")
    public void createObject(@PathVariable String bucketName, @RequestBody BucketObjectRepresentaion representaion) throws Exception {
        s3Service.putObject(bucketName, representaion);
    }

    /**
     * downloadObject
     *
     * @ignore 忽略
     */
    @GetMapping(value = "/{bucketName}/objects/{objectName}")
    public File downloadObject(@PathVariable String bucketName, @PathVariable String objectName) throws IOException {
        s3Service.downloadObject(bucketName, objectName);
        return new File("./" + objectName);
    }

    /**
     * moveObject
     *
     * @ignore 忽略
     */
    @PatchMapping(value = "/{bucketName}/objects/{objectName}/{bucketSource}")
    public void moveObject(@PathVariable String bucketName, @PathVariable String objectName, @PathVariable String bucketSource) throws IOException {
        s3Service.moveObject(bucketName, objectName, bucketSource);
    }

    /**
     * uploadImg
     *
     * @ignore 忽略
     */
    @PostMapping(value = "/{bucketName}/objectsImg")
    public void uploadImg(@PathVariable String bucketName, @RequestPart(value = "multipartFile", required = false) MultipartFile multipartFile) throws IOException {
        s3Service.putImage(bucketName, multipartFile, "");
    }

}
