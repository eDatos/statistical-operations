package org.siemac.metamac.statistical_operations.rest.external.invocation;

import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.common_metadata.rest.internal.v1_0.domain.Configuration;
import org.siemac.metamac.rest.exception.RestException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("commonMetadataRestInternalFacade")
public class CommonMetadataRestInternalFacadeImpl implements CommonMetadataRestInternalFacade {

    @Autowired
    private MetamacApisLocator restApiLocator;

    @Override
    public Configuration retrieveConfigurationById(String id) {
        try {
            return restApiLocator.getCommonMetadataRestInternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getCommonMetadataRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }
}
