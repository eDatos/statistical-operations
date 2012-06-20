package org.siemac.metamac.statistical.operations.rest.internal;

public class RestInternalConstants {

    public static String  LINK_SELF               = "self";
    public static String  LINK_SUBPATH_OPERATIONS = "operations";
    public static String  LINK_SUBPATH_FAMILIES   = "families";

    private static String API_NAME                = "statisticalOperations";
    private static String KIND_SEPARATOR          = "#";
    public static String  KIND_OPERATION          = API_NAME + KIND_SEPARATOR + "operation";
    public static String  KIND_FAMILY             = API_NAME + KIND_SEPARATOR + "family";
}