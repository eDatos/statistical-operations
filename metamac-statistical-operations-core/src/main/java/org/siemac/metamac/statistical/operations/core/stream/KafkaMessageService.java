package org.siemac.metamac.statistical.operations.core.stream;

import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface KafkaMessageService {
    void sendMessage(Operation operation);
}
