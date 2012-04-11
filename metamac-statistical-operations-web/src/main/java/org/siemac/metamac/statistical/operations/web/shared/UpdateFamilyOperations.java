package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class UpdateFamilyOperations {

    @In(1)
    Long                   familyId;

    @In(2)
    List<Long>             operationsToAdd;

    @In(3)
    List<Long>             operationsToRemove;

    @Out(1)
    List<OperationBaseDto> operationDtos;

}
