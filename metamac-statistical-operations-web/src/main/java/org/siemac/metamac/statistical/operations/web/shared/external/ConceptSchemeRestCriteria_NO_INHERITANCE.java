package org.siemac.metamac.statistical.operations.web.shared.external;

import java.io.Serializable;

import org.siemac.metamac.web.common.shared.criteria.SrmExternalResourceRestCriteria;

// FIXME METAMAC-2235 RENAME THIS CLASS when the ConceptSchemeRestCriteria were deleted
public class ConceptSchemeRestCriteria_NO_INHERITANCE implements Serializable {

    private static final long               serialVersionUID = 1L;

    private SrmExternalResourceRestCriteria criteria;
    private ConceptSchemeTypeEnum[]         conceptSchemeTypes;
    private String                          statisticalOperationUrn;

    public ConceptSchemeRestCriteria_NO_INHERITANCE() {
    }

    public ConceptSchemeRestCriteria_NO_INHERITANCE(SrmExternalResourceRestCriteria criteria) {
        setCriteria(criteria);
    }

    public ConceptSchemeRestCriteria_NO_INHERITANCE(SrmExternalResourceRestCriteria criteria, ConceptSchemeTypeEnum[] conceptSchemeTypes, String statisticalOperationUrn) {
        setCriteria(criteria);
        setConceptSchemeTypes(conceptSchemeTypes);
        setStatisticalOperationUrn(statisticalOperationUrn);
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

    public SrmExternalResourceRestCriteria getCriteria() {
        return criteria;
    }

    public void setCriteria(SrmExternalResourceRestCriteria criteria) {
        this.criteria = criteria;
    }
}
