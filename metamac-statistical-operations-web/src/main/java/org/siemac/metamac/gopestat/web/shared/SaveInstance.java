package org.siemac.metamac.gopestat.web.shared;

import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class SaveInstance {

    @In(1)
    Long        operationId;

    @In(2)
    InstanceDto instanceDto;

    @Out(1)
    InstanceDto instanceSaved;

}
