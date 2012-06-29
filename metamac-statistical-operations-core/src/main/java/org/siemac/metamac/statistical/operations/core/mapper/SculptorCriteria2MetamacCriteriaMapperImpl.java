package org.siemac.metamac.statistical.operations.core.mapper;

import java.util.ArrayList;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaResult;
import org.siemac.metamac.core.common.criteria.mapper.SculptorCriteria2MetamacCriteria;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SculptorCriteria2MetamacCriteriaMapperImpl implements SculptorCriteria2MetamacCriteriaMapper {

    @Autowired
    private Do2DtoMapper do2DtoMapper;

    @Override
    public MetamacCriteriaResult<FamilyBaseDto> pageResultToMetamacCriteriaResultFamily(PagedResult<Family> source, Integer pageSize) {
        MetamacCriteriaResult<FamilyBaseDto> target = new MetamacCriteriaResult<FamilyBaseDto>();
        target.setPaginatorResult(SculptorCriteria2MetamacCriteria.sculptorResultToMetamacCriteriaResult(source, pageSize));
        if (source.getValues() != null) {
            target.setResults(new ArrayList<FamilyBaseDto>());
            for (Family item : source.getValues()) {
                target.getResults().add(do2DtoMapper.familyToBaseDto(item));
            }
        }
        return target;
    }

    @Override
    public MetamacCriteriaResult<OperationBaseDto> pageResultToMetamacCriteriaResultOperation(PagedResult<Operation> source, Integer pageSize) {
        MetamacCriteriaResult<OperationBaseDto> target = new MetamacCriteriaResult<OperationBaseDto>();
        target.setPaginatorResult(SculptorCriteria2MetamacCriteria.sculptorResultToMetamacCriteriaResult(source, pageSize));
        if (source.getValues() != null) {
            target.setResults(new ArrayList<OperationBaseDto>());
            for (Operation item : source.getValues()) {
                target.getResults().add(do2DtoMapper.operationToBaseDto(item));
            }
        }
        return target;
    }

    @Override
    public MetamacCriteriaResult<InstanceBaseDto> pageResultToMetamacCriteriaResultInstance(PagedResult<Instance> source, Integer pageSize) {
        MetamacCriteriaResult<InstanceBaseDto> target = new MetamacCriteriaResult<InstanceBaseDto>();
        target.setPaginatorResult(SculptorCriteria2MetamacCriteria.sculptorResultToMetamacCriteriaResult(source, pageSize));
        if (source.getValues() != null) {
            target.setResults(new ArrayList<InstanceBaseDto>());
            for (Instance item : source.getValues()) {
                target.getResults().add(do2DtoMapper.instanceToBaseDto(item));
            }
        }
        return target;
    }

}
