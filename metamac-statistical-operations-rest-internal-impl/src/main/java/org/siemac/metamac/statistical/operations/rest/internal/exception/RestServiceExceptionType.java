package org.siemac.metamac.statistical.operations.rest.internal.exception;

import org.siemac.metamac.rest.exception.RestCommonServiceExceptionType;

public class RestServiceExceptionType extends RestCommonServiceExceptionType {

    public static final RestCommonServiceExceptionType FAMILY_NOT_FOUND    = create("exception.rest-operations.family.not_found");
    public static final RestCommonServiceExceptionType OPERATION_NOT_FOUND = create("exception.rest-operations.operation.not_found");
    public static final RestCommonServiceExceptionType INSTANCE_NOT_FOUND  = create("exception.rest-operations.instance.not_found");
}