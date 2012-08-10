package org.siemac.metamac.statistical_operations.rest.internal.invocation;

import org.siemac.metamac.rest.common_metadata_internal.v1_0.domain.Configuration;

public interface CommonMetadataRestInternalFacade {

    Configuration retrieveConfigurationById(String id);
}
