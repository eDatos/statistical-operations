package org.siemac.metamac.statistical.operations.core.criteria;

public enum InstanceCriteriaPropertyEnum  {

    CODE,
    TITLE,
    OPERATION_CODE,
    OPERATION_ID;
    
    public String value() {
        return name();
    }
    
    public static InstanceCriteriaPropertyEnum fromValue(String v) {
        return valueOf(v);
    }
}