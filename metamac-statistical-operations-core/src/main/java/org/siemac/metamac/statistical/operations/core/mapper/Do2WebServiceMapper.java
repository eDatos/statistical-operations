package org.siemac.metamac.statistical.operations.core.mapper;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBaseList;

public interface Do2WebServiceMapper {

    // Entities
    public OperationBase operationToOperationBase(Operation source) throws MetamacException;
    public OperationBaseList operationsToOperationBaseList(List<Operation> sources) throws MetamacException;

    // Exceptions
    MetamacExceptionFault metamacExceptionToMetamacExceptionFault(MetamacException source);
}
