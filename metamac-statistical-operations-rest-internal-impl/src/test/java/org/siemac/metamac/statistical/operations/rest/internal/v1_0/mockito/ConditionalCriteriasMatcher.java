package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import java.util.Arrays;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria.Operator;
import org.mockito.ArgumentMatcher;

public abstract class ConditionalCriteriasMatcher extends ArgumentMatcher<List<ConditionalCriteria>> {

    @SuppressWarnings("rawtypes")
    public boolean matches(List<ConditionalCriteria> expected, Object actual) {
        if (actual == null || !(actual instanceof List)) {
            return false;
        }
        if (((List) actual).size() != expected.size()) {
            return false;
        }

        for (int i = 0; i < ((List) actual).size(); i++) {
            Object actualConditionalCriteria = ((List) actual).get(i);
            if (!(actualConditionalCriteria instanceof ConditionalCriteria)) {
                return false;
            }
            ConditionalCriteria expectedConditionalCriteria = expected.get(i);
            if (!isEqualsConditionalCriteria(expectedConditionalCriteria, (ConditionalCriteria)actualConditionalCriteria)) {
                return false;
            }
        }
        return true;
    }

    private boolean isEqualsConditionalCriteria(ConditionalCriteria expected, ConditionalCriteria actual) {
        if (!expected.getOperator().equals(actual.getOperator())) {
            return false;
        }
        if (Operator.DistinctRoot.equals(expected.getOperator())) {
            return true;
        }
        if (!expected.getPropertyFullName().equals(actual.getPropertyFullName())) {
            return false;
        }
        if (!Arrays.equals(toArray(expected.getFirstOperant()), toArray(actual.getFirstOperant()))) {
            return false;
        }
        return true;
    }

    private Object[] toArray(Object object) {
        if (!(object instanceof Object[])) {
            return new Object[]{object};
        } else {
            return ((Object[]) object);
        }
    }
}