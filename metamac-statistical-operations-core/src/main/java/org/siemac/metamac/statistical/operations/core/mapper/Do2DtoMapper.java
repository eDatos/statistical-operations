package org.siemac.metamac.statistical.operations.core.mapper;

import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
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

public interface Do2DtoMapper {

    // Lists
    public SurveyTypeDto surveyTypeToDto(SurveyType surveyType);
    public InstanceTypeDto instanceTypeToDto(InstanceType instanceType);
    public SurveySourceDto surveySourceToDto(SurveySource sourceData);
    public OfficialityTypeDto officialityTypeToDto(OfficialityType officialityType);
    public CollMethodDto collMethodToDto(CollMethod collMethod);
    public CostDto costToDto(Cost cost);

    // Entities
    public FamilyDto familyToDto(Family family);
    public OperationDto operationToDto(Operation operation);
    public InstanceDto instanceToDto(Instance instance);

    public FamilyBaseDto familyToBaseDto(Family family);
    public OperationBaseDto operationToBaseDto(Operation operation);
    public InstanceBaseDto instanceToBaseDto(Instance instance);
}
