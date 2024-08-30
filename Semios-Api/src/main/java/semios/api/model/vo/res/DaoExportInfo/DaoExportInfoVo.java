package semios.api.model.vo.res.DaoExportInfo;

import lombok.Data;
import semios.api.model.entity.Dao;
import semios.api.model.vo.req.DaoExportInfoParam.DaoExportParam;

@Data
public class DaoExportInfoVo {

    // Seed Nodes Asset Type
    private SeedNodesType seedNodesType;


    // Nodes Block Parameters
    private NodeBlockParam nodeBlockParam;


    // Nodes Works Parameters
    private NodeWorksParam nodeWorksParam;


    // Nodes Tokenomics Parameters
    private NodesTokenStructure nodesTokenStructure;


    // Nodes Strategies
    private NodesStrategies nodesStrategies;


    public static DaoExportInfoVo tranferDaoExportInfoVo(Dao dao, DaoExportParam daoExportParam) {
        DaoExportInfoVo daoExportInfoVo = new DaoExportInfoVo();
        daoExportInfoVo.setSeedNodesType(SeedNodesType.tranferSeedNodesType(dao));
        daoExportInfoVo.setNodeBlockParam(NodeBlockParam.transferNodeBlockParam(dao));
        daoExportInfoVo.setNodeWorksParam(NodeWorksParam.transferNodeWorksParam(dao));
        daoExportInfoVo.setNodesTokenStructure(NodesTokenStructure.tranferNodesTokenStructure(dao,daoExportParam));
        daoExportInfoVo.setNodesStrategies(NodesStrategies.transferNodesStrategies(dao));
        return daoExportInfoVo;
    }

}
