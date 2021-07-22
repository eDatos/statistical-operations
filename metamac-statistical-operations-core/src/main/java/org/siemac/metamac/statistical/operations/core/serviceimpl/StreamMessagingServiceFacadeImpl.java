package org.siemac.metamac.statistical.operations.core.serviceimpl;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.serviceimpl.result.SendStreamMessageResult;
import org.springframework.stereotype.Service;

/**
 * Implementation of StreamMessagingServiceFacade.
 */
@Service("streamMessagingServiceFacade")
public class StreamMessagingServiceFacadeImpl extends StreamMessagingServiceFacadeImplBase {
    @Override
    public SendStreamMessageResult sendMessage(ServiceContext ctx, Operation operation) {
        return getStreamMessagingService().sendMessage(ctx, operation);
    }

    @Override
    public void resendAllPendingAndFailedMessages(ServiceContext ctx) throws MetamacException {
        getStreamMessagingService().resendAllPendingAndFailedMessages(ctx);
    }
}
