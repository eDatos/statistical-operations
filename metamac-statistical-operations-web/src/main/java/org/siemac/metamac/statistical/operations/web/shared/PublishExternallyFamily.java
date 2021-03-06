package org.siemac.metamac.statistical.operations.web.shared;

import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class PublishExternallyFamily {

    @In(1)
    Long      familyId;

    @Out(1)
    FamilyDto familySaved;

}
