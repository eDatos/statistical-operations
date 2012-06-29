package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetOperationAndInstancesByCode {

    @In(1)
    String                operationCode;

    @Out(1)
    OperationDto          operationDto;

    @Out(2)
    List<InstanceBaseDto> instanceBaseDtos;

    @Out(3)
    List<FamilyBaseDto>   familyBaseDtos;

}
