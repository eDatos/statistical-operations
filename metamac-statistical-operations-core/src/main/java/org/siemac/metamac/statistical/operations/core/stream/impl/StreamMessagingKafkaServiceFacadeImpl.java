package org.siemac.metamac.statistical.operations.core.stream.impl;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.enume.domain.StreamMessageStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.stream.StreamMessagingService;
import org.siemac.metamac.statistical.operations.core.stream.StreamMessagingServiceFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class StreamMessagingKafkaServiceFacadeImpl implements StreamMessagingServiceFacade {
    @Autowired
    public StreamMessagingService messagingService;

    @Override
    public void sendOperation(Operation operation) throws MetamacException {
        try {
            updateMessageStatus(operation, StreamMessageStatusEnum.PENDING);
            messagingService.sendMessage(operation);
            updateMessageStatus(operation, StreamMessageStatusEnum.SENT);
        } catch (MetamacException e) {
            updateMessageStatus(operation, StreamMessageStatusEnum.FAILED);
            throw new MetamacException(e, ServiceExceptionType.UNABLE_TO_SEND_STREAM_MESSAGING_TO_STREAM_MESSAGING_SERVER);
        }
    }

    private void updateMessageStatus(Operation operation, StreamMessageStatusEnum status) {
        if (operation != null) {
            operation.setStreamMessageStatus(status);
        }
    }
}
