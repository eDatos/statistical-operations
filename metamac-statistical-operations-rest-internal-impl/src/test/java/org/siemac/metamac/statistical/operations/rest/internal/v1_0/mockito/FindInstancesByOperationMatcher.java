package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.siemac.metamac.rest.common.test.mockito.ConditionalCriteriasMatcher;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;

public class FindInstancesByOperationMatcher extends ConditionalCriteriasMatcher {

    private String operationCode;

    public FindInstancesByOperationMatcher(String operationCode) {
        this.operationCode = operationCode;
    }

    public boolean matches(Object actual) {
        List<ConditionalCriteria> expected = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Instance.class)
                .withProperty(InstanceProperties.operation().code()).eq(operationCode).withProperty(InstanceProperties.procStatus())
                .in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
        return super.matches(expected, actual);
    }
}