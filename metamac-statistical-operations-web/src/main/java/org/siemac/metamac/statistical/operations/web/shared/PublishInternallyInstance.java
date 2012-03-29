package org.siemac.metamac.gopestat.web.shared;

import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class PublishInternallyInstance {

    @In(1)
    Long        instanceId;

    @Out(1)
    InstanceDto instanceSaved;

}
