package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;

public class ConceptSchemeRestCriteria extends ExternalResourceWebCriteria {

    private static final long       serialVersionUID = 1L;

    private ConceptSchemeTypeEnum[] conceptSchemeTypes;
    private String                  statisticalOperationUrn;

    public ConceptSchemeRestCriteria() {
    }

    public ConceptSchemeTypeEnum[] getConceptSchemeTypes() {
        return conceptSchemeTypes;
    }

    public void setConceptSchemeTypes(ConceptSchemeTypeEnum... conceptSchemeTypes) {
        this.conceptSchemeTypes = conceptSchemeTypes;
    }

    public String getStatisticalOperationUrn() {
        return statisticalOperationUrn;
    }

    public void setStatisticalOperationUrn(String statisticalOperationUrn) {
        this.statisticalOperationUrn = statisticalOperationUrn;
    }
}
