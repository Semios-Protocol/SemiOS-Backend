package semios.api.model.dto.common;

import lombok.Data;

import javax.xml.bind.annotation.XmlRootElement;

/**
 * @description: CreateBucketConfiguration
 * @author: xiangbin
 * @create: 2022-08-16 14:20
 **/
@XmlRootElement(name = "CreateBucketConfiguration", namespace = "http://s3.amazonaws.com/doc/2006-03-01/")
@Data
public class CreateBucketConfiguration {

    private String locationConstraint;
}
