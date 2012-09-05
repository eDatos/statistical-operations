package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface CommonMetadataRestExternalFacade {

    Configuration retrieveConfigurationById(String id) throws MetamacWebException;

    Configurations findConfigurations(String query) throws MetamacWebException;

}