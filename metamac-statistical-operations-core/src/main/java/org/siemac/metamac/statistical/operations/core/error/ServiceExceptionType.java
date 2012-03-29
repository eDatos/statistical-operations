package org.siemac.metamac.statistical.operations.core.error;

import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;

public class ServiceExceptionType extends CommonServiceExceptionType {

    // Extended Error Codes
    public static final CommonServiceExceptionType INVALID_PROC_STATUS                             = create("exception.statistical.operations.invalid.proc_status");
    public static final CommonServiceExceptionType INVALID_URL                                     = create("exception.statistical.operations.invalid.url");

    public static final CommonServiceExceptionType FAMILY_WITHOUT_PUBLISHED_INTERNALLY_OPERATIONS  = create("exception.statistical.operations.family.without_published_internally_operations");
    public static final CommonServiceExceptionType FAMILY_NOT_FOUND                                = create("exception.statistical.operations.family.not_found");
    public static final CommonServiceExceptionType FAMILY_WITHOUT_PUBLISHED_EXTERNALLY_OPERATIONS  = create("exception.statistical.operations.family.without_published_externally_operations");
    public static final CommonServiceExceptionType FAMILY_WITHOUT_OPERATIONS                       = create("exception.statistical.operations.family.without_operations");
    public static final CommonServiceExceptionType FAMILY_ALREADY_EXIST_CODE_DUPLICATED            = create("exception.statistical.operations.family.already_exist.code_duplicated");

    public static final CommonServiceExceptionType OPERATION_NOT_FOUND                             = create("exception.statistical.operations.operation.not_found");
    public static final CommonServiceExceptionType OPERATION_PUBLISH_INTERNALLY_ERROR              = create("exception.operation.publish_internally.error");
    public static final CommonServiceExceptionType OPERATION_ALREADY_EXIST_CODE_DUPLICATED         = create("exception.statistical.operations.operation.already_exist.code_duplicated");

    public static final CommonServiceExceptionType INSTANCE_INCORRECT_OPERATION_ID                 = create("exception.statistical.operations.instance.incorrect_operation_id");
    public static final CommonServiceExceptionType INSTANCE_INCORRECT_OPERATION_PROC_STATUS        = create("exception.statistical.operations.instance.incorrect_operation_proc_status");
    public static final CommonServiceExceptionType INSTANCE_NOT_FOUND                              = create("exception.statistical.operations.instance.not_found");
    public static final CommonServiceExceptionType INSTANCE_WITHOUT_OPERATION                      = create("exception.statistical.operations.instance.without_operation");
    public static final CommonServiceExceptionType INSTANCE_WITHOUT_OPERATION_PUBLISHED            = create("exception.statistical.operations.instance.without_operation_published");
    public static final CommonServiceExceptionType INSTANCE_WITHOUT_OPERATION_PUBLISHED_EXTERNALLY = create("exception.statistical.operations.instance.without_operation_published_externally");
    public static final CommonServiceExceptionType INSTANCE_ALREADY_EXIST_CODE_DUPLICATED          = create("exception.statistical.operations.instance.already_exist.code_duplicated");

    public static final CommonServiceExceptionType SURVEY_TYPE_NOT_FOUND                           = create("exception.statistical.operations.list.survey_type.not_found");
    public static final CommonServiceExceptionType ACTIVITY_TYPE_NOT_FOUND                         = create("exception.statistical.operations.list.activity_type.not_found");
    public static final CommonServiceExceptionType COLL_METHOD_NOT_FOUND                           = create("exception.statistical.operations.list.coll_method.not_found");
    public static final CommonServiceExceptionType SOURCE_DATA_NOT_FOUND                           = create("exception.statistical.operations.list.sources_data.not_found");
    public static final CommonServiceExceptionType OFFICIALITY_TYPE_NOT_FOUND                      = create("exception.statistical.operations.list.officiality_types.not_found");
    public static final CommonServiceExceptionType COST_NOT_FOUND                                  = create("exception.statistical.operations.list.cost.not_found");

    public static final CommonServiceExceptionType COMMON_METADATA_NOT_FOUND                       = create("exception.statistical.operations.common_metadata.not_found");

}
