package org.siemac.metamac.statistical_operations.rest.internal.invocation;

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.core.common.aop.LoggingInterceptor;
import org.siemac.metamac.core.common.util.shared.UrnUtils;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Concepts;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ResourceInternal;
import org.siemac.metamac.statistical_operations.rest.internal.exception.RestServiceExceptionType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("srmRestInternalFacade")
public class SrmRestInternalFacadeImpl implements SrmRestInternalFacade {

    private final Logger       logger = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Autowired
    private MetamacApisLocator restApiLocator;

    @Override
    public List<ResourceInternal> retrieveConceptsByConceptScheme(String urn) {
        try {
            String[] urnSplited = UrnUtils.splitUrnItemScheme(urn);
            String agencyID = urnSplited[0];
            String resourceID = urnSplited[1];
            String version = urnSplited[2];
            String limit = "1000";
            int offset = 0;
            List<ResourceInternal> results = new ArrayList<ResourceInternal>();
            Concepts concepts = null;
            do {
                concepts = restApiLocator.getSrmRestInternalFacadeV10().findConcepts(agencyID, resourceID, version, null, null, limit, String.valueOf(offset));
                results.addAll(concepts.getConcepts());
                offset += concepts.getConcepts().size(); // next page
            } while (concepts.getTotal().intValue() != results.size());
            return results;
        } catch (ServerWebApplicationException e) {
            logger.error("Error", e);
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            if (exception == null) {
                exception = RestExceptionUtils.getException(RestServiceExceptionType.UNKNOWN);
            }
            throw new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }
}
