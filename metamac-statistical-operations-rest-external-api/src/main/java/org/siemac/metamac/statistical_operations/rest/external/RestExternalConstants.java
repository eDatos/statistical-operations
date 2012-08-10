package org.siemac.metamac.statistical_operations.rest.external;

import org.siemac.metamac.rest.constants.RestConstants;

public class RestExternalConstants {

    public static String LINK_SUBPATH_OPERATIONS = "operations";
    public static String LINK_SUBPATH_FAMILIES   = "families";
    public static String LINK_SUBPATH_INSTANCES  = "instances";

    public static String API_NAME                = "statisticalOperations";
    public static String API_VERSION_1_0         = "v1.0";

    public static String KIND_OPERATIONS         = API_NAME + RestConstants.KIND_SEPARATOR + "operations";
    public static String KIND_OPERATION          = API_NAME + RestConstants.KIND_SEPARATOR + "operation";
    public static String KIND_FAMILIES           = API_NAME + RestConstants.KIND_SEPARATOR + "families";
    public static String KIND_FAMILY             = API_NAME + RestConstants.KIND_SEPARATOR + "family";
    public static String KIND_INSTANCE           = API_NAME + RestConstants.KIND_SEPARATOR + "instance";
    public static String KIND_INSTANCES          = API_NAME + RestConstants.KIND_SEPARATOR + "instances";
    public static String KIND_SURVEY_TYPES       = API_NAME + RestConstants.KIND_SEPARATOR + "surveyTypes";
    public static String KIND_OFFICIALITY_TYPES  = API_NAME + RestConstants.KIND_SEPARATOR + "officialityTypes";
    public static String KIND_INSTANCE_TYPES     = API_NAME + RestConstants.KIND_SEPARATOR + "instanceTypes";
    public static String KIND_SURVEY_SOURCES     = API_NAME + RestConstants.KIND_SEPARATOR + "surveySources";
    public static String KIND_COLL_METHODS       = API_NAME + RestConstants.KIND_SEPARATOR + "collMethods";
    public static String KIND_COSTS              = API_NAME + RestConstants.KIND_SEPARATOR + "costs";
}