package org.siemac.metamac.statistical.operations.web.client.view.handlers;

import org.siemac.metamac.web.common.client.view.handlers.BaseUiHandlers;

public interface ExternalUiHandlers extends BaseUiHandlers {

    void retrieveCategorySchemes(String formItemName, int firstResult, int maxResults, String criteria);
    void retrieveCategories(String formItemName, int firstResult, int maxResults, String criteria, String categorySchemeUrn);
}
