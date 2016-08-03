package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveySourceDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveyTypeDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetOperationsLists {

    @Out(1)
    List<SurveyTypeDto>      surveyTypeDtos;

    @Out(2)
    List<InstanceTypeDto>    instanceTypeDtos;

    @Out(3)
    List<SurveySourceDto>    surveySourceDtos;

    @Out(4)
    List<OfficialityTypeDto> officialityTypeDtos;

    @Out(5)
    List<CollMethodDto>      collMethodDtos;

    @Out(6)
    List<CostDto>            costDtos;

}
