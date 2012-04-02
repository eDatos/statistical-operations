package org.siemac.metamac.statistical.operations.core.serviceimpl.utils;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.exception.utils.ExceptionUtils;
import org.siemac.metamac.core.common.serviceimpl.utils.ValidationUtils;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationCriteria;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.ProcStatusType;

public class WebserviceFacadeValidationUtil {

    public static void validateFindOperations(OperationCriteria criteria) throws MetamacException {

        List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();

        ValidationUtils.checkParameterRequired(criteria, "CRITERIA", exceptions);
        if (criteria != null) {
            ValidationUtils.checkParameterRequired(criteria.isIsIndicatorsSystem(), "CRITERIA.IS_INDICATORS_SYSTEM", exceptions);
            if (criteria.getProcStatus() != null) {
                if (!ProcStatusType.PUBLISH_INTERNALLY.equals(criteria.getProcStatus()) && !ProcStatusType.PUBLISH_EXTERNALLY.equals(criteria.getProcStatus())) {
                    exceptions.add(new MetamacExceptionItem(ServiceExceptionType.PARAMETER_UNEXPECTED, criteria.getProcStatus()));
                }
            }
        }

        ExceptionUtils.throwIfException(exceptions);
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
