package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.serviceapi.ExternalItemValidator;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishExternallyInstanceActionHandler extends SecurityActionHandler<PublishExternallyInstanceAction, PublishExternallyInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Autowired
    private ExternalItemValidator              externalItemValidator;

    public PublishExternallyInstanceActionHandler() {
        super(PublishExternallyInstanceAction.class);
    }

    @Override
    public PublishExternallyInstanceResult executeSecurityAction(PublishExternallyInstanceAction action) throws ActionException {
        try {
            InstanceDto instanceToPublish = statisticalOperationsServiceFacade.findInstanceById(ServiceContextHolder.getCurrentServiceContext(), action.getInstanceId());
            checkExternalItemsAreExternallyPublished(instanceToPublish);

            InstanceDto instancePublished = statisticalOperationsServiceFacade.publishExternallyInstance(ServiceContextHolder.getCurrentServiceContext(), action.getInstanceId());
            return new PublishExternallyInstanceResult(instancePublished);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    /**
     * Check that all the external items of an {@link InstanceDto} are externally published
     * 
     * @param instanceDto
     * @throws MetamacWebException
     */
    private void checkExternalItemsAreExternallyPublished(InstanceDto instanceDto) throws MetamacWebException {
        externalItemValidator.checkExternalItemsAreExternallyPublished(ServiceExceptionParameters.INSTANCE_STATISTICAL_UNIT, instanceDto.getStatisticalUnit());
        // TODO
    }
}
