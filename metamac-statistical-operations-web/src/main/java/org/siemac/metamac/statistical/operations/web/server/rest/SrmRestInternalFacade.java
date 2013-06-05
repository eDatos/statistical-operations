package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface SrmRestInternalFacade {

    ExternalItemsResult findCategorySchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCategories(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findCodelists(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCodes(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findConceptSchemes(ConceptSchemeRestCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findConcepts(ConceptRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationUnitSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisationUnits(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findDataProviderSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findDataProviders(SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationSchemes(OrganisationSchemeRestCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisations(OrganisationRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;
}
