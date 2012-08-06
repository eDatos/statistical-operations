package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import java.util.ArrayList;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.siemac.metamac.rest.common.test.mockito.ConditionalCriteriasMatcher;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;

public class FindInstancesByOperationMatcher extends ConditionalCriteriasMatcher {

    private String                    operationCode;
    private List<ConditionalCriteria> conditionalCriteriaQueries;
    private List<ConditionalCriteria> conditionalCriteriaOrderBy;

    public FindInstancesByOperationMatcher(String operationCode, List<ConditionalCriteria> conditionalCriteriaQueries, List<ConditionalCriteria> conditionalCriteriaOrderBy) {
        this.operationCode = operationCode;
        this.conditionalCriteriaQueries = conditionalCriteriaQueries;
        this.conditionalCriteriaOrderBy = conditionalCriteriaOrderBy;
    }

    public boolean matches(Object actual) {

        List<ConditionalCriteria> expected = new ArrayList<ConditionalCriteria>();

        // By operation and procStatus
        expected.add(ConditionalCriteriaBuilder.criteriaFor(Instance.class).withProperty(InstanceProperties.operation().code()).eq(operationCode)
                .buildSingle());
        expected.add(ConditionalCriteriaBuilder.criteriaFor(Instance.class).withProperty(InstanceProperties.procStatus())
                .in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle());

        // orderBy
        if (conditionalCriteriaOrderBy != null) {
            expected.addAll(conditionalCriteriaOrderBy);
        } else {
            // default order
            expected.add(ConditionalCriteriaBuilder.criteriaFor(Instance.class).orderBy(InstanceProperties.id()).ascending().buildSingle());
        }
        // query
        if (conditionalCriteriaQueries != null) {
            expected.addAll(conditionalCriteriaQueries);
        }
        // distinc root
        expected.add(ConditionalCriteriaBuilder.criteriaFor(Instance.class).distinctRoot().buildSingle());

        // Compare
        return super.matches(expected, actual);
    }
}