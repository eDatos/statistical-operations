package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;

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
