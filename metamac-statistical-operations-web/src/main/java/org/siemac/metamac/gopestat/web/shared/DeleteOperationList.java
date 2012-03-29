package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;

@GenDispatch(isSecure = false)
public class DeleteOperationList {

    @In(1)
    List<Long> operationIds;

}
