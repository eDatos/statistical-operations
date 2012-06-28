package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito;

import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.mockito.ArgumentMatcher;

public class FindFamiliesByOperationCode1PaginatorMatcher extends ArgumentMatcher<PagingParameter> {

    public boolean matches(Object other) {
        if (other == null || !(other instanceof PagingParameter)) {
            return false;
        }
        PagingParameter pagingParameter = (PagingParameter) other;
        if (pagingParameter.getStartRow() != PagingParameter.UNKNOWN) {
            return false;
        }
        if (pagingParameter.getStartRow() != PagingParameter.UNKNOWN) {
            return false;
        }
        if (pagingParameter.getPageSize() != PagingParameter.UNKNOWN) {
            return false;
        }
        return true;
    }
}