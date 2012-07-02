package org.siemac.metamac.statistical.operations.rest.internal;

public class RestInternalConstants {

    public static String  LINK_SUBPATH_OPERATIONS = "operations";
    public static String  LINK_SUBPATH_FAMILIES   = "families";
    public static String  LINK_SUBPATH_INSTANCES  = "instances";

    private static String API_NAME                = "statisticalOperations";

    private static String KIND_SEPARATOR          = "#";
    public static String  KIND_OPERATIONS         = API_NAME + KIND_SEPARATOR + "operations";
    public static String  KIND_OPERATION          = API_NAME + KIND_SEPARATOR + "operation";
    public static String  KIND_FAMILIES           = API_NAME + KIND_SEPARATOR + "families";
    public static String  KIND_FAMILY             = API_NAME + KIND_SEPARATOR + "family";
    public static String  KIND_INSTANCE           = API_NAME + KIND_SEPARATOR + "instance";
    public static String  KIND_INSTANCES          = API_NAME + KIND_SEPARATOR + "instances";
}
