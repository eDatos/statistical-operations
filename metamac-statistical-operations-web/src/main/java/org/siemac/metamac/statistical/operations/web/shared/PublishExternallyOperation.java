package org.siemac.metamac.statistical.operations.web.shared;

import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class PublishExternallyOperation {

    @In(1)
    Long                operationId;

    @Out(1)
    OperationDto        operationSaved;

    @Out(2)
    MetamacWebException notificationException;

}
