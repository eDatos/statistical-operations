package org.siemac.metamac.statistical.operations.core.mapper;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;

public interface Dto2DoMapper {

    // List Entities
    public SurveyType surveyTypeDtoToEntity(SurveyTypeDto source, ServiceContext ctx) throws MetamacException;
    public InstanceType instanceTypeDtoToEntity(InstanceTypeDto source, ServiceContext ctx) throws MetamacException;
    public SurveySource surveySourceDtoToEntity(SurveySourceDto source, ServiceContext ctx) throws MetamacException;
    public OfficialityType officialityTypeDtoToEntity(OfficialityTypeDto source, ServiceContext ctx) throws MetamacException;
    public CollMethod collMethodDtoToEntity(CollMethodDto source, ServiceContext ctx) throws MetamacException;
    public Cost costDtoToEntity(CostDto source, ServiceContext ctx) throws MetamacException;

    // Entities
    public Family familyDtoToEntity(FamilyDto source, ServiceContext ctx) throws MetamacException;
    public Operation operationDtoToEntity(OperationDto source, ServiceContext ctx) throws MetamacException;
    public Instance instanceDtoToEntity(InstanceDto source, ServiceContext ctx) throws MetamacException;
}
