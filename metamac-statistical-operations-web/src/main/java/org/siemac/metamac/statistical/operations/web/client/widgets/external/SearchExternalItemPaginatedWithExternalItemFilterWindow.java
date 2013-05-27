package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.statistical.operations.web.client.widgets.SearchRelatedResourceBasePaginatedWithRelatedResourceFilterWindow;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;

public class SearchExternalItemPaginatedWithExternalItemFilterWindow extends SearchRelatedResourceBasePaginatedWithRelatedResourceFilterWindow<ExternalItemDto> {

    public SearchExternalItemPaginatedWithExternalItemFilterWindow(String title, String filterListTitle, String filterTextTitle, String selectionListTitle, int maxResults,
            PaginatedAction filterListAction, PaginatedAction selectionListAction) {
        super(title, filterListTitle, filterTextTitle, selectionListTitle, maxResults, filterListAction, selectionListAction);
    }
}
