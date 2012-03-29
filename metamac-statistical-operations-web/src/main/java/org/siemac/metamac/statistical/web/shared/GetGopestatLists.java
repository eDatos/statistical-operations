package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.CollMethodDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.CostDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceTypeDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.SurveySourceDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.SurveyTypeDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetGopestatLists {

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
