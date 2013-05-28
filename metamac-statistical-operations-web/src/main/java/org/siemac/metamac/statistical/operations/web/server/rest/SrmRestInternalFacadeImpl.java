package org.siemac.metamac.statistical.operations.web.server.rest;

import static org.siemac.metamac.srm.rest.internal.RestInternalConstants.WILDCARD;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Categories;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Codelists;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Codes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ConceptSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Concepts;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.DataProviderSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.DataProviders;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnitSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnits;
import org.siemac.metamac.statistical.operations.web.server.rest.utils.RestCriteriaUtils;
import org.siemac.metamac.statistical.operations.web.server.utils.ExternalItemUtils;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;
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
    // CODELISTS
    //

    @Override
    public ExternalItemsResult findCodelists(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildCodelistQuery(criteria);

        try {
            Codelists codelists = restApiLocator.getSrmRestInternalFacadeV10().findCodelists(query, orderBy, limit, offset);
            return ExternalItemUtils.getCodelistsAsExternalItemsResult(codelists);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding codelists");
        }
    }

    //
    // CODES
    //

    @Override
    public ExternalItemsResult findCodes(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildCodeQuery(itemWebCriteria);

        try {
            Codes codes = restApiLocator.getSrmRestInternalFacadeV10().findCodes(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getCodesAsExternalItemsResult(codes);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding codes");
        }
    }

    //
    // CONCEPT SCHEMES
    //

    @Override
    public ExternalItemsResult findConceptSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildConceptSchemeQuery(criteria);

        try {
            ConceptSchemes conceptSchemes = restApiLocator.getSrmRestInternalFacadeV10().findConceptSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getConceptSchemesAsExternalItemsResult(conceptSchemes);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding concept schemes");
        }
    }

    //
    // CONCEPTS
    //

    @Override
    public ExternalItemsResult findConcepts(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildConceptQuery(itemWebCriteria);

        try {
            Concepts concepts = restApiLocator.getSrmRestInternalFacadeV10().findConcepts(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getConceptsAsExternalItemsResult(concepts);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding concepts");
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

    //
    // DATA PROVIDER SCHEMES
    //

    @Override
    public ExternalItemsResult findDataProviderSchemes(ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildOrganisationSchemeQuery(criteria);

        try {
            DataProviderSchemes dataProviderSchemes = restApiLocator.getSrmRestInternalFacadeV10().findDataProviderSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getDataProviderSchemesAsExternalItemsResult(dataProviderSchemes);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding data provider schemes");
        }
    }

    //
    // DATA PROVIDERS
    //

    @Override
    public ExternalItemsResult findDataProviders(ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestCriteriaUtils.buildOrganisationQuery(itemWebCriteria);

        try {

            DataProviders dataProviders = restApiLocator.getSrmRestInternalFacadeV10().findDataProviders(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getDataProvidersAsExternalItemsResult(dataProviders);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                    org.siemac.metamac.rest.common.v1_0.domain.Exception.class);
            throw WebExceptionUtils.createMetamacWebException(exception);
        } catch (Exception e) {
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "Error finding data providers");
        }
    }

    //
    // ORGANISATION SCHEMES
    //

    @Override
    public ExternalItemsResult findOrganisiatonSchemes(OrganisationSchemeWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {
        // TODO Auto-generated method stub
        return null;
    }

    //
    // ORGANISATIONS
    //

    @Override
    public ExternalItemsResult findOrganisations(OrganisationWebCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {
        // TODO Auto-generated method stub
        return null;
    }
}
