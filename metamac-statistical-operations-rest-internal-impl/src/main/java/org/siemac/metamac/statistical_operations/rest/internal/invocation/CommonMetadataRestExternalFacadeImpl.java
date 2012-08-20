package org.siemac.metamac.statistical_operations.rest.internal.invocation;

import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.core.common.aop.LoggingInterceptor;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.statistical_operations.rest.internal.exception.RestServiceExceptionType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("commonMetadataRestExternalFacade")
public class CommonMetadataRestExternalFacadeImpl implements CommonMetadataRestExternalFacade {

    private Logger             logger = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Autowired
    private MetamacApisLocator restApiLocator;

    @Override
    public Configuration retrieveConfigurationById(String id) {
        try {
            return restApiLocator.getCommonMetadataRestExternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
        } catch (ServerWebApplicationException e) {
            logger.error("Error", e);
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getCommonMetadataRestExternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            if (exception == null) {
                exception = RestExceptionUtils.getException(RestServiceExceptionType.UNKNOWN);
            }
            throw new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }
}
