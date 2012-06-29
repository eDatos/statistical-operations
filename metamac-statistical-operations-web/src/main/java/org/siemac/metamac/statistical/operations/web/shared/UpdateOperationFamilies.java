package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class UpdateOperationFamilies {

    @In(1)
    Long                operationId;

    @In(2)
    List<Long>          familiesToAdd;

    @In(3)
    List<Long>          familiesToRemove;

    @Out(1)
    List<FamilyBaseDto> familyBaseDtos;

}
