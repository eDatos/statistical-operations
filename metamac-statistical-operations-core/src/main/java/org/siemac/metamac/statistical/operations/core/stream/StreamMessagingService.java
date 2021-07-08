package org.siemac.metamac.statistical.operations.core.stream;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface StreamMessagingService {
    void sendMessage(Operation operation) throws MetamacException;
}
