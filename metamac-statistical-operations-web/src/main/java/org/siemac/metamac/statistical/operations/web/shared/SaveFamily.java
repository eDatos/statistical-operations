package org.siemac.metamac.statistical.operations.web.shared;

import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class SaveFamily {

    @In(1)
    FamilyDto family;

    @Out(1)
    FamilyDto familySaved;

}
