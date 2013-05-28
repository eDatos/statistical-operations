package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface SrmRestInternalFacade {

    ExternalItemsResult findCategorySchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCategories(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findCodelists(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCodes(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findConceptSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findConcepts(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationUnitSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisationUnits(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findDataProviderSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findDataProviders(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationSchemes(OrganisationSchemeWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisations(OrganisationWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;
}
