package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;

public class SearchMultipleConceptsAndConceptSchemesItem extends SearchMultipleItemsItem {

    public SearchMultipleConceptsAndConceptSchemesItem(String name, String title, ExternalResourceWebCriteria itemSchemeCriteria, ItemWebCriteria itemCriteria, MultipleExternalResourceAction action) {
        super(name, title, itemSchemeCriteria, itemCriteria, getConstants().searchConcepts(), getConstants().filterConceptScheme(), getConstants().selectedConceptScheme(), getConstants()
                .selectionConcepts(), action);
    }
}
