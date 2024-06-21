package semios.api.model.vo.res;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import semios.api.SemiosApiApplication;
import semios.api.mapper.DaoMapper;
import semios.api.model.entity.Dao;


import static org.junit.jupiter.api.Assertions.*;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = SemiosApiApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@Transactional
class TogetherDaoListVoTest {

    @Autowired
    private DaoMapper daoMapper;

    @Test
    void transfer() {
        Dao dao = daoMapper.selectById(1);
        TogetherDaoListVo togetherDaoListVo = TogetherDaoListVo.transfer(dao);
        assertEquals(1, togetherDaoListVo.getDaoId(), "成功");
    }

    @Test
    void transferTogetherDaoListVo() {
        Dao dao = daoMapper.selectById(1);
        TogetherDaoListVo togetherDaoListVo = TogetherDaoListVo.transfer(dao);
        assertEquals(1, togetherDaoListVo.getDaoId(), "成功");
    }
}