package org.siemac.metamac.statistical.operations.core.serviceapi;

import org.fornax.cartridges.sculptor.framework.test.AbstractDbUnitJpaTests;
import org.junit.Test;

/**
 * Spring based transactional test with DbUnit support.
 */
public class StreamMessagingServiceTest extends AbstractDbUnitJpaTests implements StreamMessagingServiceTestBase {
    @Test
    public void testSendMessage() {
    }

    @Test
    public void testResendAllPendingAndFailedMessages() {
    }
}
