package org.siemac.metamac.statistical.operations.web.server.rest;

import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;

public interface SrmRestInternalFacade {

    ExternalItemsResult findCategorySchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCategories(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findCodelists(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findCodes(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisationUnitSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisationUnits(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findDataProviderSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findDataProviders(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;

    ExternalItemsResult findOrganisiatonSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException;
    ExternalItemsResult findOrganisations(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException;
}
