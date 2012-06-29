package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.mockito.ArgumentMatcher;

public class PagingParameterMatcher extends ArgumentMatcher<PagingParameter> {

    private PagingParameter pagingParameter;

    public PagingParameterMatcher(PagingParameter pagingParameter) {
        this.pagingParameter = pagingParameter;
    }

    public boolean matches(Object other) {
        if (other == null || !(other instanceof PagingParameter)) {
            return false;
        }
        PagingParameter pagingParameterToMatch = (PagingParameter) other;
        if (pagingParameter.getStartRow() != pagingParameterToMatch.getStartRow()) {
            return false;
        }
        if (pagingParameter.getEndRow() != pagingParameterToMatch.getEndRow()) {
            return false;
        }
        if (pagingParameter.getPageSize() != pagingParameterToMatch.getPageSize()) {
            return false;
        }
        return true;
    }
}