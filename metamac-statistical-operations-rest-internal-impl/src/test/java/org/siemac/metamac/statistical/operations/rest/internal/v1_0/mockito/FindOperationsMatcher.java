package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;

public class FindOperationsMatcher extends ConditionalCriteriasMatcher {

    public boolean matches(Object actual) {
        List<ConditionalCriteria> expected = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                .withProperty(OperationProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
        return super.matches(expected, actual);
    }
}