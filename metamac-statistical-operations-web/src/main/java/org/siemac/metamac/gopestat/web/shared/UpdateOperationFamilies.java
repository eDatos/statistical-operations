package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;

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
