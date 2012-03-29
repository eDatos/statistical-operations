package org.siemac.metamac.statistical.operations.core.mapper;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveySourceDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveyTypeDto;

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
