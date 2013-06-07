package org.siemac.metamac.statistical.operations.core.criteria;

public enum InstanceCriteriaOrderEnum {

    CODE, NAME, LAST_UPDATED;

    public String value() {
        return name();
    }

    public static InstanceCriteriaOrderEnum fromValue(String v) {
        return valueOf(v);
    }
}