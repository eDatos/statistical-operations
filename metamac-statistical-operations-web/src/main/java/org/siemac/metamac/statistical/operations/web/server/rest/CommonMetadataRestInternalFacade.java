package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.common.metadata.rest.internal.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface CommonMetadataRestInternalFacade {

    Configuration retrieveConfigurationById(String id) throws MetamacWebException;

    ResourcesNoPagedResult findConfigurations(String query) throws MetamacWebException;

}
