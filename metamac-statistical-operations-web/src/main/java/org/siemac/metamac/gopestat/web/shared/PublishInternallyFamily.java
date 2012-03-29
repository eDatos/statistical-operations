package org.siemac.metamac.gopestat.web.shared;

import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class PublishInternallyFamily {

    @In(1)
    Long      familyId;

    @Out(1)
    FamilyDto familySaved;

}
