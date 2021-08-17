package org.siemac.metamac.statistical.operations.web.server.rest;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface NoticesRestInternalFacade {

    String BEAN_ID = "noticesRestInternalFacade";

    void createNotificationForPublishInternallyOperation(ServiceContext serviceContext, OperationDto operation) throws MetamacWebException;

    void createNotificationForPublishExternallyOperation(ServiceContext serviceContext, OperationDto operation) throws MetamacWebException;

    void createNotificationForStreamError(ServiceContext serviceContext, OperationDto operation) throws MetamacWebException;
}
