package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationBase;

public interface Do2RestInternalMapper {

    // Entities
    public OperationBase operationToOperationBase(Operation source) throws MetamacException;
}
