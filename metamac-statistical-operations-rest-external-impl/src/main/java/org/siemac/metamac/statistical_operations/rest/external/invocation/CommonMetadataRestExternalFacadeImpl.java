package org.siemac.metamac.statistical_operations.rest.external.invocation;

import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("commonMetadataRestExternalFacade")
public class CommonMetadataRestExternalFacadeImpl implements CommonMetadataRestExternalFacade {

    private final Logger       logger = LoggerFactory.getLogger(CommonMetadataRestExternalFacadeImpl.class);

    @Autowired
    private MetamacApisLocator restApiLocator;

    @Override
    public Configuration retrieveConfigurationById(String id) {
        try {
            return restApiLocator.getCommonMetadataRestExternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
        } catch (Exception e) {
            throw toRestException(e);
        }
    }

    private RestException toRestException(Exception e) {
        logger.error("Error", e);
        return RestExceptionUtils.toRestException(e, WebClient.client(restApiLocator.getCommonMetadataRestExternalFacadeV10()));
    }
}
