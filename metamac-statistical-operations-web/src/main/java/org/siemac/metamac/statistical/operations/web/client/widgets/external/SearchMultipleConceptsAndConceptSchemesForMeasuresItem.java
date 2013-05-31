package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;

public class SearchMultipleConceptsAndConceptSchemesForMeasuresItem extends SearchMultipleConceptsAndConceptSchemesItem {

    public SearchMultipleConceptsAndConceptSchemesForMeasuresItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildMeasuresConceptSchemeWebCriteria(), RestWebCriteriaUtils.buildMeasuresConceptWebCriteria(), action);
    }
}
