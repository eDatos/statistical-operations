package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.web.common.client.widgets.SearchMultipleRelatedResourceBasePaginatedWithRelatedResourceFilterWindow;
import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;

/**
 * Window with a {@link SearchRelatedResourcePaginatedDragAndDropItem} and a {@link SearchRelatedResourcePaginatedItem} as a filter.
 */
public class SearchMultipleExternalItemPaginatedWithExternalItemFilterWindow extends SearchMultipleRelatedResourceBasePaginatedWithRelatedResourceFilterWindow<ExternalItemDto> {

    public SearchMultipleExternalItemPaginatedWithExternalItemFilterWindow(String title, String filterListTitle, String filterTextTitle, String selectionListTitle, int maxResults,
            PaginatedAction filterListAction, PaginatedAction selectionListAction) {
        super(title, filterListTitle, filterTextTitle, selectionListTitle, maxResults, filterListAction, selectionListAction);
    }
}
