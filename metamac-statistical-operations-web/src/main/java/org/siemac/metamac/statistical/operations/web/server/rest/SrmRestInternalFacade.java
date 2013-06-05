package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface SrmRestInternalFacade {

    ExternalItemsResult findCategorySchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCategories(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findCodelists(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCodes(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findConceptSchemes(ConceptSchemeWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findConcepts(ConceptWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationUnitSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisationUnits(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findDataProviderSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findDataProviders(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationSchemes(OrganisationSchemeWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisations(OrganisationWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;
}
