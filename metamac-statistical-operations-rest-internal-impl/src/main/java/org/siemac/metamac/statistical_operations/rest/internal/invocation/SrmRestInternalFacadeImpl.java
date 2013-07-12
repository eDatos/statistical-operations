package org.siemac.metamac.statistical_operations.rest.internal.invocation;

import java.util.ArrayList;
import java.util.List;

import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.core.common.util.shared.UrnUtils;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Concepts;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ResourceInternal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("srmRestInternalFacade")
public class SrmRestInternalFacadeImpl implements SrmRestInternalFacade {

    private final Logger       logger = LoggerFactory.getLogger(SrmRestInternalFacadeImpl.class);

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
        } catch (Exception e) {
            throw toRestException(e);
        }
    }

    private RestException toRestException(Exception e) {
        logger.error("Error", e);
        return RestExceptionUtils.toRestException(e, WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()));
    }
}
