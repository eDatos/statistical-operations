package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public interface Do2RestInternalMapperV10 {

    // Entities
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl) throws MetamacException;
}
