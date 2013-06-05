package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

public class ConceptWebCriteria extends SrmItemRestCriteria {

    private static final long       serialVersionUID = 1L;

    private ConceptSchemeTypeEnum[] conceptSchemeTypes;
    private String                  statisticalOperationUrn;

    public ConceptWebCriteria() {
    }

    public ConceptSchemeTypeEnum[] getConceptSchemeTypes() {
        return conceptSchemeTypes;
    }

    public String getStatisticalOperationUrn() {
        return statisticalOperationUrn;
    }

    public void setConceptSchemeTypes(ConceptSchemeTypeEnum... conceptSchemeTypes) {
        this.conceptSchemeTypes = conceptSchemeTypes;
    }

    public void setStatisticalOperationUrn(String statisticalOperationUrn) {
        this.statisticalOperationUrn = statisticalOperationUrn;
    }
}
