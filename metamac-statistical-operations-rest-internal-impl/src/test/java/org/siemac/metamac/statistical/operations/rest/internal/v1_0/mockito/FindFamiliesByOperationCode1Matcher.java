package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import java.util.Arrays;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria.Operator;
import org.mockito.ArgumentMatcher;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.service.StatisticalOperationsRestFacadeV10Test;

public class FindFamiliesByOperationCode1Matcher extends ArgumentMatcher<List<ConditionalCriteria>> {

    @SuppressWarnings("unchecked")
    public boolean matches(Object other) {
        if (other == null || !(other instanceof List)) {
            return false;
        }
        List<ConditionalCriteria> otherList = (List<ConditionalCriteria>) other;
        if (otherList.size() != 2) {
            return false;
        }
        for (Object otherObject : otherList) {
            if (!(otherObject instanceof ConditionalCriteria)) {
                return false;
            }
        }

        // Condition 1, by operation
        if (!isEqualsConditionalCriteria(otherList.get(0), FamilyProperties.operations().code().getName(), Operator.Equal, StatisticalOperationsRestFacadeV10Test.OPERATION_CODE1)) {
            return false;
        }
        // Condition 2, by status
        if (!isEqualsConditionalCriteria(otherList.get(1), FamilyProperties.procStatus().getName(), Operator.In, ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY)) {
            return false;
        }
        return true;
    }

    private Boolean isEqualsConditionalCriteria(ConditionalCriteria conditionalCriteriaActual, String propertyFullNameExpected, Operator operatorExpected, Object... firstOperantExpected) {
        if (!conditionalCriteriaActual.getPropertyFullName().equals(propertyFullNameExpected)) {
            return false;
        }
        if (!conditionalCriteriaActual.getOperator().equals(operatorExpected)) {
            return false;
        }
        Object[] firstOperantActual = null;
        if (!(conditionalCriteriaActual.getFirstOperant() instanceof Object[])) {
            firstOperantActual = new Object[]{conditionalCriteriaActual.getFirstOperant()};
        } else {
            firstOperantActual = ((Object[])conditionalCriteriaActual.getFirstOperant());
        }
        
        if (!Arrays.equals(firstOperantActual, firstOperantExpected)) {
            return false;
        }
        return true;
    }
}