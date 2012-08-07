package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.common_metadata.rest.internal.v1_0.domain.Configuration;
import org.siemac.metamac.common_metadata.rest.internal.v1_0.domain.Configurations;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface CommonMetadataRestInternalFacade {

    Configuration retrieveConfigurationById(String id) throws MetamacWebException;

    Configurations findConfigurations(String query) throws MetamacWebException;

}
