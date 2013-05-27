package org.siemac.metamac.statistical.operations.web.server.rest;

import static org.siemac.metamac.srm.rest.internal.RestInternalConstants.WILDCARD;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Categories;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnitSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnits;
import org.siemac.metamac.statistical.operations.web.server.rest.utils.RestCriteriaUtils;
import org.siemac.metamac.statistical.operations.web.server.utils.ExternalItemUtils;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.constants.CommonSharedConstants;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SrmRestInternalFacadeImpl implements SrmRestInternalFacade {

    @Autowired
    private RestApiLocator restApiLocator;

    //
    // CATEGORY SCHEMES
    //

    @Override
    public ExternalItemsResult findCategorySchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildCategorySchemeQuery(criteria);

        try {
            CategorySchemes categorySchemes = restApiLocator.getSrmRestInternalFacadeV10().findCategorySchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getCategorySchemesAsExternalItemsResult(categorySchemes);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding category schemes");
        }
    }

    //
    // CATEGORIES
    //

    @Override
    public ExternalItemsResult findCategories(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildCategoryQuery(itemWebCriteria);

        try {
            Categories categories = restApiLocator.getSrmRestInternalFacadeV10().findCategories(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getCategoriesAsExternalItemsResult(categories);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding category schemes");
        }
    }

    //
    // ORGANISATION UNIT SCHEMES
    //

    @Override
    public ExternalItemsResult findOrganisationUnitSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildOrganisationSchemeQuery(criteria);

        try {
            OrganisationUnitSchemes organisationUnitSchemes = restApiLocator.getSrmRestInternalFacadeV10().findOrganisationUnitSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getOrganisationUnitSchemesAsExternalItemsResult(organisationUnitSchemes);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding organisation unit schemes");
        }

    }

    //
    // ORGANISATION UNITS
    //

    @Override
    public ExternalItemsResult findOrganisationUnits(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildOrganisationQuery(itemWebCriteria);

        try {

            OrganisationUnits organisationUnits = restApiLocator.getSrmRestInternalFacadeV10().findOrganisationUnits(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getOrganisationUnitsAsExternalItemsResult(organisationUnits);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding organisation units");
        }
    }
}
