package org.siemac.metamac.statistical.operations.web.server.rest.serviceapi;

import java.util.Set;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface ExternalItemValidator {

    public static final String BEAN_ID = "externalItemValidator";

    public void checkExternalItemIsExternallyPublished(String externalItemName, ExternalItemDto externalItemDto) throws MetamacWebException;
    public void checkExternalItemsAreExternallyPublished(String externalItemName, Set<ExternalItemDto> externalItemDtos) throws MetamacWebException;
}