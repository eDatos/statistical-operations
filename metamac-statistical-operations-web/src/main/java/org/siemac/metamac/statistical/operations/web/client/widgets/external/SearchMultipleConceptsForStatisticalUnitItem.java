package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;

public class SearchMultipleConceptsForStatisticalUnitItem extends SearchMultipleSrmItemsItem {

    public SearchMultipleConceptsForStatisticalUnitItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildStatisticalUnitConceptSchemeWebCriteria(), RestWebCriteriaUtils.buildStatisticalUnitConceptWebCriteria(), getConstants().searchConcepts(),
                getConstants().filterConceptScheme(), getConstants().selectedConceptScheme(), getConstants().selectionConcepts(), action);
    }
}
