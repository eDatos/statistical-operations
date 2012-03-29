package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class UpdateInstancesOrder {

    @In(1)
    Long                  operationId;

    @In(2)
    List<Long>            instancesIds;

    @Out(1)
    List<InstanceBaseDto> instances;

}
