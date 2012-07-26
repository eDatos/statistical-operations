package org.siemac.metamac.statistical.operations.core.criteria;

public enum OperationCriteriaPropertyEnum  {

    CODE,
    TITLE,
    ACRONYM,
    DESCRIPTION,
    PROC_STATUS,
    FAMILY_CODE,
    FAMILY_ID;
    
    public String value() {
        return name();
    }
    
    public static OperationCriteriaPropertyEnum fromValue(String v) {
        return valueOf(v);
    }
}