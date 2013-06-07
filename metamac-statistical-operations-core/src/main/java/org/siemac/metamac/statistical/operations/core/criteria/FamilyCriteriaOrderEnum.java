package org.siemac.metamac.statistical.operations.core.criteria;

public enum FamilyCriteriaOrderEnum {

    CODE, NAME, LAST_UPDATED;

    public String value() {
        return name();
    }

    public static FamilyCriteriaOrderEnum fromValue(String v) {
        return valueOf(v);
    }
}