package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetFamilyAndOperations {

    @In(1)
    String                 familyUrn;

    @Out(1)
    FamilyDto              familyDto;

    @Out(2)
    List<OperationBaseDto> operationBaseDtos;

}
