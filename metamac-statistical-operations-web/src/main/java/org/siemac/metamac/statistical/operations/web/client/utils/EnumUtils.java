package org.siemac.metamac.statistical.operations.web.client.utils;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;

import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;

public class EnumUtils {

    private static Set<StatusEnum>               statusEnums;
    private static LinkedHashMap<String, String> statusEnumHashMap;

    /**
     * Return FacetValueTypeEnums that represents time
     * 
     * @return
     */
    public static Set<StatusEnum> getStatusEnums() {
        if (statusEnums == null) {
            statusEnums = new HashSet<StatusEnum>();
            statusEnums.add(StatusEnum.PLANNING);
            statusEnums.add(StatusEnum.DESIGN);
            statusEnums.add(StatusEnum.PRODUCTION);
            statusEnums.add(StatusEnum.OUT_OF_PRINT);
        }
        return statusEnums;
    }

    public static LinkedHashMap<String, String> getStatusEnumHashMap() {
        if (statusEnumHashMap == null) {
            statusEnumHashMap = new LinkedHashMap<String, String>();
            for (StatusEnum s : getStatusEnums()) {
                String value = OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().statusEnum() + s.getName());
                statusEnumHashMap.put(s.toString(), value);
            }
        }
        return statusEnumHashMap;
    }

}
