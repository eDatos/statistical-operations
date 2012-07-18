package org.siemac.metamac.statistical.operations.web.server.rest;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.common.metadata.rest.internal.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.constants.CommonSharedConstants;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CommonMetadataRestInternalFacadeImpl implements CommonMetadataRestInternalFacade {

    @Autowired
    private RestApiLocator restApiLocator;

    @Override
    public Configuration retrieveConfigurationById(String id) throws MetamacWebException {
        try {
            return restApiLocator.getCommonMetadataRestInternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = e.toErrorObject(WebClient.client(restApiLocator.getCommonMetadataRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Error.class);
            throw WebExceptionUtils.createMetamacWebException(error);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, OperationsWeb.getCoreMessages().exception_common_unknown());
        }
    }

    @Override
    public ResourcesNoPagedResult findConfigurations(String query) throws MetamacWebException {
        try {
            // Only returns enabled configurations
            return restApiLocator.getCommonMetadataRestInternalFacadeV10().findConfigurations(query, null);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = e.toErrorObject(WebClient.client(restApiLocator.getCommonMetadataRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Error.class);
            throw WebExceptionUtils.createMetamacWebException(error);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, OperationsWeb.getCoreMessages().exception_common_unknown());
        }
    }

}
