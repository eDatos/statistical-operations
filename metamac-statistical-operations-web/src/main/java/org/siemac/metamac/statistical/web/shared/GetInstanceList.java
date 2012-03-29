package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetInstanceList {

    @In(1)
    Long                  operationId;

    @Out(1)
    List<InstanceBaseDto> instanceBaseDtos;

}
