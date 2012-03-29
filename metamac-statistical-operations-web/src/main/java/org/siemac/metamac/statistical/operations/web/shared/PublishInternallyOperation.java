package org.siemac.metamac.gopestat.web.shared;

import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class PublishInternallyOperation {

    @In(1)
    Long         operationId;

    @Out(1)
    OperationDto operationSaved;

}
