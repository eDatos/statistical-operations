package org.siemac.metamac.statistical.operations.web.server.rest;

import javax.annotation.PostConstruct;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.common_metadata.rest.external.v1_0.service.CommonMetadataV1_0;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.notices.rest.internal.v1_0.service.NoticesV1_0;
import org.siemac.metamac.srm.rest.internal.v1_0.service.SrmRestInternalFacadeV10;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RestApiLocator {

    @Autowired
    private ConfigurationService     configurationService;

    private CommonMetadataV1_0       commonMetadataRestExternalFacadeV10 = null;
    private SrmRestInternalFacadeV10 srmRestInternalFacadeV10            = null;
    private NoticesV1_0              noticesRestInternalFacadeV10        = null;

    @PostConstruct
    public void initService() throws Exception {

        String commonMetadataBaseApi = configurationService.retrieveCommonMetadataExternalApiUrlBase();
        commonMetadataRestExternalFacadeV10 = JAXRSClientFactory.create(commonMetadataBaseApi, CommonMetadataV1_0.class, null, true); // true to do thread safe

        String srmBaseApi = configurationService.retrieveSrmInternalApiUrlBase();
        srmRestInternalFacadeV10 = JAXRSClientFactory.create(srmBaseApi, SrmRestInternalFacadeV10.class, null, true);

        String noticesBaseApi = configurationService.retrieveNoticesInternalApiUrlBase();
        noticesRestInternalFacadeV10 = JAXRSClientFactory.create(noticesBaseApi, NoticesV1_0.class, null, true);
    }

    public CommonMetadataV1_0 getCommonMetadataRestExternalFacadeV10() {
        // reset thread context
        WebClient.client(commonMetadataRestExternalFacadeV10).reset();
        WebClient.client(commonMetadataRestExternalFacadeV10).accept("application/xml");

        return commonMetadataRestExternalFacadeV10;
    }

    public SrmRestInternalFacadeV10 getSrmRestInternalFacadeV10() {
        // reset thread context
        WebClient.client(srmRestInternalFacadeV10).reset();
        WebClient.client(srmRestInternalFacadeV10).accept("application/xml");

        return srmRestInternalFacadeV10;
    }

    public NoticesV1_0 getNoticesRestInternalFacadeV10() {
        // reset thread context
        WebClient.client(noticesRestInternalFacadeV10).reset();
        WebClient.client(noticesRestInternalFacadeV10).accept("application/xml");

        return noticesRestInternalFacadeV10;
    }
}
