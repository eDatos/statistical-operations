package org.siemac.metamac.gopestat.web.shared;

import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class SaveOperation {

    @In(1)
    OperationDto operationDto;

    @Out(1)
    OperationDto operationSaved;

}
