package org.siemac.metamac.statistical.operations.web.server.rest;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.web.common.shared.criteria.CommonConfigurationRestCriteria;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface CommonMetadataRestExternalFacade {

    Configuration retrieveConfigurationById(ServiceContext serviceContext, String id) throws MetamacWebException;
    Configurations findConfigurations(ServiceContext serviceContext, CommonConfigurationRestCriteria criteria) throws MetamacWebException;
}
