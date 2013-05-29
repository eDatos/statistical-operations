package org.siemac.metamac.statistical.operations.web.client.view.handlers;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.web.common.client.view.handlers.BaseUiHandlers;

public interface ExternalResourcesUiHandlers extends BaseUiHandlers {

    void retrieveItemSchemes(String formItemName, TypeExternalArtefactsEnum[] types, int firstResult, int maxResults, String criteria);
    void retrieveItemSchemes(String formItemName, ExternalResourceWebCriteria externalResourceWebCriteria, int firstResult, int maxResults);

    void retrieveItems(String formItemName, TypeExternalArtefactsEnum[] types, int firstResult, int maxResults, String criteria, String itemSchemeUrn);
    void retrieveItems(String formItemName, ItemWebCriteria itemWebCriteria, int firstResult, int maxResults);
}
