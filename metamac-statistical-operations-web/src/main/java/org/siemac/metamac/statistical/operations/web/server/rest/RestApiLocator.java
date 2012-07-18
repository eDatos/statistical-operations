package org.siemac.metamac.statistical.operations.web.server.rest;

import javax.annotation.PostConstruct;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.common.metadata.rest.internal.v1_0.service.CommonMetadataRestInternalFacadeV10;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RestApiLocator {

    @Autowired
    private ConfigurationService                configurationService;

    private CommonMetadataRestInternalFacadeV10 commonMetadataRestInternalFacadeV10 = null;

    @PostConstruct
    public void initService() throws Exception {
        String baseApi = configurationService.getProperties().getProperty(RestApiConstants.COMMON_METADATA_REST_INTERNAL);
        commonMetadataRestInternalFacadeV10 = JAXRSClientFactory.create(baseApi, CommonMetadataRestInternalFacadeV10.class, null, true); // true to do thread safe
    }

    public CommonMetadataRestInternalFacadeV10 getCommonMetadataRestInternalFacadeV10() {
        // reset thread context
        WebClient.client(commonMetadataRestInternalFacadeV10).reset();
        WebClient.client(commonMetadataRestInternalFacadeV10).accept("application/xml");

        return commonMetadataRestInternalFacadeV10;
    }

}
