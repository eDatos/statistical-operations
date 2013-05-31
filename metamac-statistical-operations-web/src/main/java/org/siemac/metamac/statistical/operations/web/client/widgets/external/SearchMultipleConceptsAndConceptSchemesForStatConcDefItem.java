package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;

public class SearchMultipleConceptsAndConceptSchemesForStatConcDefItem extends SearchMultipleConceptsAndConceptSchemesItem {

    public SearchMultipleConceptsAndConceptSchemesForStatConcDefItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildStatConcDefConceptSchemeWebCriteria(), RestWebCriteriaUtils.buildStatConcDefConceptWebCriteria(), action);
    }
}
