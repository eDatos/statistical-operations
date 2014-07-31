package org.siemac.metamac.statistical.operations.web.shared.external;

import java.io.Serializable;

import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

// FIXME METAMAC-2235 RENAME THIS CLASS when the ConceptRestCriteria were deleted
public class ConceptRestCriteria_NO_INHERITANCE implements Serializable {

    private static final long       serialVersionUID = 1L;

    private SrmItemRestCriteria     criteria;
    private ConceptSchemeTypeEnum[] conceptSchemeTypes;
    private String                  statisticalOperationUrn;

    public ConceptRestCriteria_NO_INHERITANCE() {
    }

    public ConceptRestCriteria_NO_INHERITANCE(SrmItemRestCriteria criteria) {
        setCriteria(criteria);
    }

    public ConceptRestCriteria_NO_INHERITANCE(SrmItemRestCriteria criteria, ConceptSchemeTypeEnum[] conceptSchemeTypeEnums, String statisticalOperationUrn) {
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
