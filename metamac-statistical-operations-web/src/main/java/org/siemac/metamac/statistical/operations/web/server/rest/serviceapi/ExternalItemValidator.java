package org.siemac.metamac.statistical.operations.web.server.rest.serviceapi;

import java.util.Set;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface ExternalItemValidator {

    public static final String BEAN_ID = "externalItemValidator";

    public void checkExternalItemIsExternallyPublished(ServiceContext serviceContext, String externalItemName, ExternalItemDto externalItemDto, MetamacWebException metamacWebException)
            throws MetamacWebException;
    public void checkExternalItemsAreExternallyPublished(ServiceContext serviceContext, String externalItemName, Set<ExternalItemDto> externalItemDtos, MetamacWebException metamacWebException)
            throws MetamacWebException;
}