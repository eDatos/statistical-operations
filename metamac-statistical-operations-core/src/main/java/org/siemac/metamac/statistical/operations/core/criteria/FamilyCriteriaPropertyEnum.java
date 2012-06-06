package org.siemac.metamac.statistical.operations.core.criteria;

public enum FamilyCriteriaPropertyEnum  {

    CODE;
    
    public String value() {
        return name();
    }
    
    public static FamilyCriteriaPropertyEnum fromValue(String v) {
        return valueOf(v);
    }
}