package org.siemac.metamac.statistical.operations.core.serviceimpl.utils;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.exception.utils.ExceptionUtils;
import org.siemac.metamac.core.common.serviceimpl.utils.ValidationUtils;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteria;

public class WebServiceFacadeValidationUtil {

    public static void validateFindOperations(MetamacCriteria criteria) throws MetamacException {
        // nothing
    }

    public static void validateRetrieveOperation(String code) throws MetamacException {
        List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();

        ValidationUtils.checkParameterRequired(code, "CODE", exceptions);

        ExceptionUtils.throwIfException(exceptions);
    }

    public static void validateRetrieveVersion() throws MetamacException {
        // nothing
    }
}
