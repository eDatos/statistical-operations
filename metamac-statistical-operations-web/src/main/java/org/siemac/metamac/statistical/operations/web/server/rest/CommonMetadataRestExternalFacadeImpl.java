package org.siemac.metamac.statistical.operations.web.server.rest;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.web.server.rest.utils.RestQueryUtils;
import org.siemac.metamac.web.common.server.rest.utils.RestExceptionUtils;
import org.siemac.metamac.web.common.shared.criteria.CommonConfigurationRestCriteria;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CommonMetadataRestExternalFacadeImpl implements CommonMetadataRestExternalFacade {

    @Autowired
    private RestApiLocator     restApiLocator;

    @Autowired
    private RestExceptionUtils restExceptionUtils;

    @Override
    public Configuration retrieveConfigurationById(ServiceContext serviceContext, String id) throws MetamacWebException {
        try {
            return restApiLocator.getCommonMetadataRestExternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
        } catch (Exception e) {
            throw manageCommonMetadataExternalRestException(serviceContext, e);
        }
    }

    @Override
    public Configurations findConfigurations(ServiceContext serviceContext, CommonConfigurationRestCriteria criteria) throws MetamacWebException {
        try {
            String query = RestQueryUtils.buildCommonConfigurationQuery(criteria);
            // Only returns enabled configurations
            return restApiLocator.getCommonMetadataRestExternalFacadeV10().findConfigurations(query, null);
        } catch (Exception e) {
            throw manageCommonMetadataExternalRestException(serviceContext, e);
        }
    }

    //
    // EXCEPTION HANDLERS
    //

    private MetamacWebException manageCommonMetadataExternalRestException(ServiceContext ctx, Exception e) throws MetamacWebException {
        return restExceptionUtils.manageMetamacRestException(ctx, e, ServiceExceptionParameters.API_COMMON_METADATA_EXTERNAL, restApiLocator.getCommonMetadataRestExternalFacadeV10());
    }

}
