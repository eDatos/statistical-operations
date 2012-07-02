package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.siemac.metamac.rest.common.test.mockito.ConditionalCriteriasMatcher;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.service.StatisticalOperationsRestFacadeV10Test;

public class FindFamiliesByOperation1Matcher extends ConditionalCriteriasMatcher {

    public boolean matches(Object actual) {
        String operationCode = StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1;
        List<ConditionalCriteria> expected = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                .withProperty(FamilyProperties.operations().code()).eq(operationCode)
                .withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
        return super.matches(expected, actual);
    }
}