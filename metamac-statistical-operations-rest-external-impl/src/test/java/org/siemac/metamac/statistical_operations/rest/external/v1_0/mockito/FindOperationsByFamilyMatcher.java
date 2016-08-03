package org.siemac.metamac.statistical_operations.rest.external.v1_0.mockito;

import java.util.ArrayList;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.siemac.metamac.rest.common.test.mockito.ConditionalCriteriasMatcher;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;

public class FindOperationsByFamilyMatcher extends ConditionalCriteriasMatcher {

    private String family;
    private List<ConditionalCriteria> conditionalCriteriaQueries;
    private List<ConditionalCriteria> conditionalCriteriaOrderBy;

    public FindOperationsByFamilyMatcher(String family, List<ConditionalCriteria> conditionalCriteriaQueries, List<ConditionalCriteria> conditionalCriteriaOrderBy) {
        this.family = family;
        this.conditionalCriteriaQueries = conditionalCriteriaQueries;
        this.conditionalCriteriaOrderBy = conditionalCriteriaOrderBy;
    }

    public boolean matches(Object actual) {

        List<ConditionalCriteria> expected = new ArrayList<ConditionalCriteria>();

        // By family and procStatus
        expected.add(ConditionalCriteriaBuilder.criteriaFor(Operation.class).withProperty(OperationProperties.families().code()).eq(family)
                .buildSingle());
        expected.add(ConditionalCriteriaBuilder.criteriaFor(Operation.class).withProperty(OperationProperties.procStatus())
                .eq(ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle());

        // orderBy
        if (conditionalCriteriaOrderBy != null) {
            expected.addAll(conditionalCriteriaOrderBy);
        } else {
            // default order
            expected.add(ConditionalCriteriaBuilder.criteriaFor(Operation.class).orderBy(OperationProperties.id()).ascending().buildSingle());
        }
        // query
        if (conditionalCriteriaQueries != null) {
            expected.addAll(conditionalCriteriaQueries);
        }
        // distinc root
        expected.add(ConditionalCriteriaBuilder.criteriaFor(Operation.class).distinctRoot().buildSingle());

        // Compare
        return super.matches(expected, actual);
    }
}