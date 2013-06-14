package org.siemac.metamac.statistical.operations.web.server.rest;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.lang.shared.LocaleConstants;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configuration;
import org.siemac.metamac.rest.common_metadata.v1_0.domain.Configurations;
import org.siemac.metamac.statistical.operations.web.server.utils.WebTranslateExceptions;
import org.siemac.metamac.statistical.operations.web.shared.constants.WebMessageExceptionsConstants;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.constants.CommonSharedConstants;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CommonMetadataRestExternalFacadeImpl implements CommonMetadataRestExternalFacade {

    private static Logger          logger = Logger.getLogger(CommonMetadataRestExternalFacadeImpl.class.getName());

    @Autowired
    private RestApiLocator         restApiLocator;

    @Autowired
    private WebTranslateExceptions webTranslateExceptions;

    @Override
    public Configuration retrieveConfigurationById(ServiceContext serviceContext, String id) throws MetamacWebException {
        try {
            return restApiLocator.getCommonMetadataRestExternalFacadeV10().retrieveConfigurationById(id); // CONFIGURATION ID in the rest API is what we call CODE
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    @Override
    public Configurations findConfigurations(ServiceContext serviceContext, String query) throws MetamacWebException {
        try {
            // Only returns enabled configurations
            return restApiLocator.getCommonMetadataRestExternalFacadeV10().findConfigurations(query, null);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // EXCEPTION HANDLERS
    //

    private void throwMetamacWebExceptionFromServerWebApplicationException(ServiceContext serviceContext, ServerWebApplicationException e) throws MetamacWebException {

        logger.log(Level.SEVERE, e.getMessage());

        org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getCommonMetadataRestExternalFacadeV10()),
                org.siemac.metamac.rest.common.v1_0.domain.Exception.class);

        if (exception == null) {
            if (e.getResponse().getStatus() == 404) {
                throwMetamacWebException(serviceContext, WebMessageExceptionsConstants.REST_API_COMMON_METADATA_INVOCATION_ERROR_404);
            } else {
                throwMetamacWebException(serviceContext, WebMessageExceptionsConstants.REST_API_COMMON_METADATA_INVOCATION_ERROR_UNKNOWN);
            }
        }

        throw WebExceptionUtils.createMetamacWebException(exception);
    }

    private void throwMetamacWebException(ServiceContext serviceContext, String exceptionCode) throws MetamacWebException {
        String locale = (String) serviceContext.getProperty(LocaleConstants.locale);
        String exceptionnMessage = webTranslateExceptions.getTranslatedMessage(exceptionCode, locale);

        throw new MetamacWebException(exceptionCode, exceptionnMessage);
    }
}
