package org.siemac.metamac.statistical.operations.web.shared.external;

import java.io.Serializable;

import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

public class ConceptRestCriteria implements Serializable {

    private static final long       serialVersionUID = 1L;

    private SrmItemRestCriteria     criteria;
    private ConceptSchemeTypeEnum[] conceptSchemeTypes;
    private String                  statisticalOperationUrn;

    public ConceptRestCriteria() {
    }

    public ConceptRestCriteria(SrmItemRestCriteria criteria) {
        setCriteria(criteria);
    }

    public ConceptRestCriteria(SrmItemRestCriteria criteria, ConceptSchemeTypeEnum[] conceptSchemeTypeEnums, String statisticalOperationUrn) {
        setCriteria(criteria);
        setConceptSchemeTypes(conceptSchemeTypeEnums);
        setStatisticalOperationUrn(statisticalOperationUrn);
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

    public SrmItemRestCriteria getCriteria() {
        return criteria;
    }

    public void setCriteria(SrmItemRestCriteria criteria) {
        this.criteria = criteria;
    }
}
