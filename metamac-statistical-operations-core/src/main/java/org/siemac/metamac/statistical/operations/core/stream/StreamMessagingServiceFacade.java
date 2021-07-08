package org.siemac.metamac.statistical.operations.core.stream;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface StreamMessagingServiceFacade {
    void sendOperation(Operation operation) throws MetamacException;
}
