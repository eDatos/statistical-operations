package org.siemac.metamac.statistical_operations.rest.external.invocation;

import org.siemac.metamac.common_metadata.rest.internal.v1_0.domain.Configuration;

public interface CommonMetadataRestInternalFacade {

    Configuration retrieveConfigurationById(String id);
}
