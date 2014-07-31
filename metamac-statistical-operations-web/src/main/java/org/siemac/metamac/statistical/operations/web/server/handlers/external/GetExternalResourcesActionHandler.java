package org.siemac.metamac.statistical.operations.web.server.handlers.external;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.statistical.operations.web.server.rest.SrmRestInternalFacade;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria_NO_INHERITANCE;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria_NO_INHERITANCE;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesResult;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeRestCriteria;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.shared.constants.CommonSharedConstants;
import org.siemac.metamac.web.common.shared.criteria.SrmExternalResourceRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetExternalResourcesActionHandler extends SecurityActionHandler<GetExternalResourcesAction, GetExternalResourcesResult> {

    @Autowired
    private SrmRestInternalFacade srmRestInternalFacade;

    public GetExternalResourcesActionHandler() {
        super(GetExternalResourcesAction.class);
    }

    @Override
    public GetExternalResourcesResult executeSecurityAction(GetExternalResourcesAction action) throws ActionException {

        ServiceContext serviceContext = ServiceContextHolder.getCurrentServiceContext();

        ExternalItemsResult result = null;
        switch (action.getExternalResourceWebCriteria().getExternalArtifactType()) {
            case CATEGORY_SCHEME:
                result = srmRestInternalFacade.findCategorySchemes(serviceContext, (SrmExternalResourceRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(),
                        action.getMaxResults());
                break;
            case CATEGORY:
                result = srmRestInternalFacade.findCategories(serviceContext, (SrmItemRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case ORGANISATION_SCHEME:
                result = srmRestInternalFacade.findOrganisationSchemes(serviceContext, (OrganisationSchemeRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(),
                        action.getMaxResults());
                break;
            case ORGANISATION:
                result = srmRestInternalFacade.findOrganisations(serviceContext, (OrganisationRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case ORGANISATION_UNIT_SCHEME:
                result = srmRestInternalFacade.findOrganisationUnitSchemes(serviceContext, (SrmExternalResourceRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(),
                        action.getMaxResults());
                break;
            case ORGANISATION_UNIT:
                result = srmRestInternalFacade.findOrganisationUnits(serviceContext, (SrmItemRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case DATA_PROVIDER_SCHEME:
                result = srmRestInternalFacade.findDataProviderSchemes(serviceContext, (SrmExternalResourceRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(),
                        action.getMaxResults());
                break;
            case DATA_PROVIDER:
                result = srmRestInternalFacade.findDataProviders(serviceContext, (SrmItemRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CODELIST:
                result = srmRestInternalFacade
                        .findCodelists(serviceContext, (SrmExternalResourceRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CODE:
                result = srmRestInternalFacade.findCodes(serviceContext, (SrmItemRestCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CONCEPT_SCHEME:
                ConceptSchemeRestCriteria_NO_INHERITANCE conceptSchemeRestCriteria = new ConceptSchemeRestCriteria_NO_INHERITANCE((SrmExternalResourceRestCriteria) action.getExternalResourceWebCriteria(),
                        action.getConceptSchemeTypes(), action.getStatisticalOperationUrn());
                result = srmRestInternalFacade.findConceptSchemes(serviceContext, conceptSchemeRestCriteria, action.getFirstResult(), action.getMaxResults());
                break;
            case CONCEPT:
                ConceptRestCriteria_NO_INHERITANCE conceptRestCriteria = new ConceptRestCriteria_NO_INHERITANCE((SrmItemRestCriteria) action.getExternalResourceWebCriteria(), action.getConceptSchemeTypes(),
                        action.getStatisticalOperationUrn());
                result = srmRestInternalFacade.findConcepts(serviceContext, conceptRestCriteria, action.getFirstResult(), action.getMaxResults());
                break;
            default:
                throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, "An unknown exception has ocurred. Please contact system administrator.");
        }
        return new GetExternalResourcesResult(result);
    }
}
