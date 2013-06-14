package org.siemac.metamac.statistical.operations.web.server.rest;

import static org.siemac.metamac.srm.rest.internal.RestInternalConstants.WILDCARD;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.lang.shared.LocaleConstants;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Categories;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.CategorySchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Codelists;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Codes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.ConceptSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Concepts;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.DataProviderSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.DataProviders;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnitSchemes;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.OrganisationUnits;
import org.siemac.metamac.rest.structural_resources_internal.v1_0.domain.Organisations;
import org.siemac.metamac.statistical.operations.web.server.rest.utils.ExternalItemUtils;
import org.siemac.metamac.statistical.operations.web.server.rest.utils.RestQueryUtils;
import org.siemac.metamac.statistical.operations.web.server.utils.WebTranslateExceptions;
import org.siemac.metamac.statistical.operations.web.shared.constants.WebMessageExceptionsConstants;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeRestCriteria;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.constants.CommonSharedConstants;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SrmRestInternalFacadeImpl implements SrmRestInternalFacade {

    private static Logger          logger = Logger.getLogger(SrmRestInternalFacadeImpl.class.getName());

    @Autowired
    private RestApiLocator         restApiLocator;

    @Autowired
    private WebTranslateExceptions webTranslateExceptions;

    //
    // CATEGORY SCHEMES
    //

    @Override
    public ExternalItemsResult findCategorySchemes(ServiceContext serviceContext, ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildCategorySchemeQuery(criteria);

        try {
            CategorySchemes categorySchemes = restApiLocator.getSrmRestInternalFacadeV10().findCategorySchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getCategorySchemesAsExternalItemsResult(categorySchemes);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // CATEGORIES
    //

    @Override
    public ExternalItemsResult findCategories(ServiceContext serviceContext, SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildCategoryQuery(itemWebCriteria);

        try {
            Categories categories = restApiLocator.getSrmRestInternalFacadeV10().findCategories(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getCategoriesAsExternalItemsResult(categories);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // CODELISTS
    //

    @Override
    public ExternalItemsResult findCodelists(ServiceContext serviceContext, SrmItemSchemeRestCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildCodelistQuery(criteria);

        try {
            Codelists codelists = restApiLocator.getSrmRestInternalFacadeV10().findCodelists(query, orderBy, limit, offset);
            return ExternalItemUtils.getCodelistsAsExternalItemsResult(codelists);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // CODES
    //

    @Override
    public ExternalItemsResult findCodes(ServiceContext serviceContext, SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildCodeQuery(itemWebCriteria);

        try {
            Codes codes = restApiLocator.getSrmRestInternalFacadeV10().findCodes(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getCodesAsExternalItemsResult(codes);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // CONCEPT SCHEMES
    //

    @Override
    public ExternalItemsResult findConceptSchemes(ServiceContext serviceContext, ConceptSchemeRestCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildConceptSchemeQuery(criteria);

        try {
            ConceptSchemes conceptSchemes = restApiLocator.getSrmRestInternalFacadeV10().findConceptSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getConceptSchemesAsExternalItemsResult(conceptSchemes);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // CONCEPTS
    //

    @Override
    public ExternalItemsResult findConcepts(ServiceContext serviceContext, ConceptRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildConceptQuery(itemWebCriteria);

        try {
            Concepts concepts = restApiLocator.getSrmRestInternalFacadeV10().findConcepts(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getConceptsAsExternalItemsResult(concepts);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // ORGANISATION UNIT SCHEMES
    //

    @Override
    public ExternalItemsResult findOrganisationUnitSchemes(ServiceContext serviceContext, ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildOrganisationSchemeQuery(criteria);

        try {
            OrganisationUnitSchemes organisationUnitSchemes = restApiLocator.getSrmRestInternalFacadeV10().findOrganisationUnitSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getOrganisationUnitSchemesAsExternalItemsResult(organisationUnitSchemes);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // ORGANISATION UNITS
    //

    @Override
    public ExternalItemsResult findOrganisationUnits(ServiceContext serviceContext, SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildOrganisationQuery(itemWebCriteria);

        try {

            OrganisationUnits organisationUnits = restApiLocator.getSrmRestInternalFacadeV10().findOrganisationUnits(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getOrganisationUnitsAsExternalItemsResult(organisationUnits);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // DATA PROVIDER SCHEMES
    //

    @Override
    public ExternalItemsResult findDataProviderSchemes(ServiceContext serviceContext, ExternalResourceWebCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildOrganisationSchemeQuery(criteria);

        try {
            DataProviderSchemes dataProviderSchemes = restApiLocator.getSrmRestInternalFacadeV10().findDataProviderSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getDataProviderSchemesAsExternalItemsResult(dataProviderSchemes);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // DATA PROVIDERS
    //

    @Override
    public ExternalItemsResult findDataProviders(ServiceContext serviceContext, SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildOrganisationQuery(itemWebCriteria);

        try {

            DataProviders dataProviders = restApiLocator.getSrmRestInternalFacadeV10().findDataProviders(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getDataProvidersAsExternalItemsResult(dataProviders);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // ORGANISATION SCHEMES
    //

    @Override
    public ExternalItemsResult findOrganisationSchemes(ServiceContext serviceContext, OrganisationSchemeRestCriteria criteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildOrganisationSchemeQuery(criteria);

        try {
            OrganisationSchemes organisationSchemes = restApiLocator.getSrmRestInternalFacadeV10().findOrganisationSchemes(query, orderBy, limit, offset);
            return ExternalItemUtils.getOrganisationSchemesAsExternalItemsResult(organisationSchemes);
        } catch (ServerWebApplicationException e) {
            throwMetamacWebExceptionFromServerWebApplicationException(serviceContext, e);
            return null;
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.getMessage());
            throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, e.getMessage());
        }
    }

    //
    // ORGANISATIONS
    //

    @Override
    public ExternalItemsResult findOrganisations(ServiceContext serviceContext, OrganisationRestCriteria itemWebCriteria, int firstResult, int maxResults) throws MetamacWebException {

        String limit = String.valueOf(maxResults);
        String offset = String.valueOf(firstResult);
        String orderBy = null;
        String query = RestQueryUtils.buildOrganisationQuery(itemWebCriteria);

        try {

            Organisations organisations = restApiLocator.getSrmRestInternalFacadeV10().findOrganisations(WILDCARD, WILDCARD, WILDCARD, query, orderBy, limit, offset);
            return ExternalItemUtils.getOrganisationsAsExternalItemsResult(organisations);
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

        org.siemac.metamac.rest.common.v1_0.domain.Exception exception = e.toErrorObject(WebClient.client(restApiLocator.getSrmRestInternalFacadeV10()),
                org.siemac.metamac.rest.common.v1_0.domain.Exception.class);

        if (exception == null) {
            if (e.getResponse().getStatus() == 404) {
                throwMetamacWebException(serviceContext, WebMessageExceptionsConstants.REST_API_SRM_INVOCATION_ERROR_404);
            } else {
                throwMetamacWebException(serviceContext, WebMessageExceptionsConstants.REST_API_SRM_INVOCATION_ERROR_UNKNOWN);
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
