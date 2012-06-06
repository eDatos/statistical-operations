package org.siemac.metamac.statistical.operations.core.criteria;

public enum OperationCriteriaPropertyEnum  {

    CODE;
    
    public String value() {
        return name();
    }
    
    public static OperationCriteriaPropertyEnum fromValue(String v) {
        return valueOf(v);
    }
}