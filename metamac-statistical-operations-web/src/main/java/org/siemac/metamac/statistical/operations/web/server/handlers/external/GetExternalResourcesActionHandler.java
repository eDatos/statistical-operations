package org.siemac.metamac.statistical.operations.web.server.handlers.external;

import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.server.rest.SrmRestInternalFacade;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesResult;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationSchemeWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.OrganisationWebCriteria;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.shared.constants.CommonSharedConstants;
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
        ExternalItemsResult result = null;
        switch (action.getExternalResourceWebCriteria().getType()) {
            case CATEGORY_SCHEME:
                result = srmRestInternalFacade.findCategorySchemes(action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CATEGORY:
                result = srmRestInternalFacade.findCategories((ItemWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case ORGANISATION_SCHEME:
                result = srmRestInternalFacade.findOrganisationSchemes((OrganisationSchemeWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case ORGANISATION:
                result = srmRestInternalFacade.findOrganisations((OrganisationWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case ORGANISATION_UNIT_SCHEME:
                result = srmRestInternalFacade.findOrganisationUnitSchemes(action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case ORGANISATION_UNIT:
                result = srmRestInternalFacade.findOrganisationUnits((ItemWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case DATA_PROVIDER_SCHEME:
                result = srmRestInternalFacade.findDataProviderSchemes(action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case DATA_PROVIDER:
                result = srmRestInternalFacade.findDataProviders((ItemWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CODELIST:
                result = srmRestInternalFacade.findCodelists(action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CODE:
                result = srmRestInternalFacade.findCodes((ItemWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CONCEPT_SCHEME:
                result = srmRestInternalFacade.findConceptSchemes((ConceptSchemeWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            case CONCEPT:
                result = srmRestInternalFacade.findConcepts((ConceptWebCriteria) action.getExternalResourceWebCriteria(), action.getFirstResult(), action.getMaxResults());
                break;
            default:
                throw new MetamacWebException(CommonSharedConstants.EXCEPTION_UNKNOWN, OperationsWeb.getCoreMessages().exception_common_unknown());
        }
        return new GetExternalResourcesResult(result);
    }
}
