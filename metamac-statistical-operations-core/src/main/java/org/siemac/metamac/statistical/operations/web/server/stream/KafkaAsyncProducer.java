package org.siemac.metamac.statistical.operations.web.server.stream;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.sso.client.MetamacPrincipalAccess;
import org.siemac.metamac.sso.client.SsoClientConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatisticalOperationsRoleEnum;
import org.siemac.metamac.statistical.operations.core.serviceapi.StreamMessagingServiceFacade;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
public class KafkaAsyncProducer implements ApplicationListener<ContextRefreshedEvent> {
    private static final Logger LOGGER = LoggerFactory.getLogger(KafkaAsyncProducer.class);

    @Autowired
    public StreamMessagingServiceFacade streamMessagingServiceFacade;

    @Override
    @Async
    public void onApplicationEvent(ContextRefreshedEvent event) {
        try {
            streamMessagingServiceFacade.resendAllPendingAndFailedMessages(createServiceContext());
        } catch (Exception e) {
            LOGGER.error("Could not send operations", e);
        }
    }

    private ServiceContext createServiceContext() {
        ServiceContext serviceContext = new ServiceContext("kafka-async-producer",
                                                           "kafka-async-producer",
                                                           StatisticalOperationsConstants.APPLICATION_ID);
        MetamacPrincipal metamacPrincipal = new MetamacPrincipal();
        metamacPrincipal.setUserId(serviceContext.getUserId());
        MetamacPrincipalAccess metamacPrincipalAccess = new MetamacPrincipalAccess(StatisticalOperationsRoleEnum.ADMINISTRADOR.getName(),
                                                                                   StatisticalOperationsConstants.APPLICATION_ID,
                                                                                   null);
        metamacPrincipal.getAccesses().add(metamacPrincipalAccess);
        serviceContext.setProperty(SsoClientConstants.PRINCIPAL_ATTRIBUTE, metamacPrincipal);
        return serviceContext;
    }
}
