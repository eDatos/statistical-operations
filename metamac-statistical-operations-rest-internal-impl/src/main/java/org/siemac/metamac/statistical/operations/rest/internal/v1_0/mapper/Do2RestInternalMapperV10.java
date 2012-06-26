package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public interface Do2RestInternalMapperV10 {

    // Entities
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl);
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl);
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl);
    
    // Other
    public Error toError(Exception exception);
}
