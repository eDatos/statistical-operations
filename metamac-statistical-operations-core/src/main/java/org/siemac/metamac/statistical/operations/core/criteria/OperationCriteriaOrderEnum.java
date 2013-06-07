package org.siemac.metamac.statistical.operations.core.criteria;

public enum OperationCriteriaOrderEnum {

    CODE, NAME, LAST_UPDATED;

    public String value() {
        return name();
    }

    public static OperationCriteriaOrderEnum fromValue(String v) {
        return valueOf(v);
    }
}