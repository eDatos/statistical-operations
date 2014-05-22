package org.siemac.metamac.statistical.operations.web.server.rest;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface NoticesRestInternalService {

    public static final String BEAN_ID = "noticesRestInternalService";

    void createNotificationForPublishInternallyOperation(ServiceContext serviceContext, OperationDto operation) throws MetamacWebException;

    void createNotificationForPublishExternallyOperation(ServiceContext serviceContext, OperationDto operation) throws MetamacWebException;

}
