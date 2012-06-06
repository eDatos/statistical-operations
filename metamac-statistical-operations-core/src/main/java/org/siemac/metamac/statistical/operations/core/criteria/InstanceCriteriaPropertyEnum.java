package org.siemac.metamac.statistical.operations.core.criteria;

public enum InstanceCriteriaPropertyEnum  {

    CODE;
    
    public String value() {
        return name();
    }
    
    public static InstanceCriteriaPropertyEnum fromValue(String v) {
        return valueOf(v);
    }
}