package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetOperationAndInstances {

    @In(1)
    Long                  operationId;

    @Out(1)
    OperationDto          operationDto;

    @Out(2)
    List<InstanceBaseDto> instanceBaseDtos;

    @Out(3)
    List<FamilyBaseDto>   familyBaseDtos;

}
