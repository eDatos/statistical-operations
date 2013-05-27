package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.web.common.client.widgets.actions.PaginatedAction;

public class SearchMultipleOrganisationsWindow extends SearchMultipleExternalItemPaginatedWithExternalItemFilterWindow {

    public SearchMultipleOrganisationsWindow(int maxResults, PaginatedAction filterListAction, PaginatedAction selectionListAction) {
        super(getConstants().searchOrganisations(), getConstants().filterOrganisationScheme(), getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(), maxResults,
                filterListAction, selectionListAction);
    }
}
