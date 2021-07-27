package org.siemac.metamac.statistical.operations.core.serviceapi;

import org.fornax.cartridges.sculptor.framework.test.AbstractDbUnitJpaTests;

/**
 * Spring based transactional test with DbUnit support.
 */
public class StreamMessagingServiceFacadeTest extends AbstractDbUnitJpaTests implements StreamMessagingServiceFacadeTestBase {
    @Override
    public void testSendMessage() {
        // NO TEST
    }

    @Override
    public void testResendAllPendingAndFailedMessages() {
        // NO TEST
    }
}
