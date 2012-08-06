package org.siemac.metamac.statistical_operations.rest.internal.invocation;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("commonMetadataRestInternalFacade")
public class CommonMetadataRestInternalFacadeImpl implements CommonMetadataRestInternalFacade {

    @Autowired
    private MetamacApisLocator restApiLocator;

    // TODO
//    @Override
//    public Configuration retrieveConfigurationById(String id) {
//        try {
//            return restApiLocator.getCommonMetadataRestInternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
//        } catch (ServerWebApplicationException e) {
//            org.siemac.metamac.rest.common.v1_0.domain.Error error = e.toErrorObject(WebClient.client(restApiLocator.getCommonMetadataRestInternalFacadeV10()),
//                    org.siemac.metamac.rest.common.v1_0.domain.Error.class);
//            throw new RestException(error, Status.INTERNAL_SERVER_ERROR);
//        }
//    }
}
