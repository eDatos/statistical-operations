package org.siemac.metamac.statistical.operations.web.client.view.handlers;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.client.view.handlers.BaseUiHandlers;

public interface ExternalResourcesUiHandlers extends BaseUiHandlers {

    void retrieveItemSchemes(String formItemName, TypeExternalArtefactsEnum[] types, int firstResult, int maxResults, String criteria);
    void retrieveItems(String formItemName, TypeExternalArtefactsEnum[] types, int firstResult, int maxResults, String criteria, String itemSchemeUrn);
}
