package org.siemac.metamac.statistical.operations.web.shared.external;

public class ConceptSchemeWebCriteria extends ExternalResourceWebCriteria {

    private static final long       serialVersionUID = 1L;

    private ConceptSchemeTypeEnum[] conceptSchemeTypes;
    private String                  statisticalOperationUrn;

    public ConceptSchemeWebCriteria() {
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
