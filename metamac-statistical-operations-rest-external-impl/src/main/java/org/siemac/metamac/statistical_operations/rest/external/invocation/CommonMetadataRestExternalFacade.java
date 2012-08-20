package org.siemac.metamac.statistical_operations.rest.external.invocation;

import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;

public interface CommonMetadataRestExternalFacade {

    Configuration retrieveConfigurationById(String id);
}
