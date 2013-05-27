package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;

public class SearchCategoryWindow extends SearchExternalItemPaginatedWithExternalItemFilterWindow {

    public SearchCategoryWindow(int maxResults, PaginatedAction filterListAction, PaginatedAction selectionListAction) {
        super(getConstants().searchCategories(), getConstants().filterCategoryScheme(), getConstants().selectedCategoryScheme(), getConstants().selectionCategories(), maxResults, filterListAction,
                selectionListAction);
    }
}
