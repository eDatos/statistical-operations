package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetFamilyAndOperations {

    @In(1)
    Long                   familyId;

    @Out(1)
    FamilyDto              familyDto;

    @Out(2)
    List<OperationBaseDto> operationBaseDtos;

}
