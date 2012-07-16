package org.siemac.metamac.statistical.operations.web.client.utils;

import java.util.LinkedHashMap;

import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;

public class CommonUtils {

    private static LinkedHashMap<String, String> statusEnumHashMap;

    public static LinkedHashMap<String, String> getStatusEnumHashMap() {
        if (statusEnumHashMap == null) {
            statusEnumHashMap = new LinkedHashMap<String, String>();
            for (StatusEnum s : StatusEnum.values()) {
                String value = OperationsWeb.getCoreMessages().getString(OperationsWeb.getCoreMessages().statusEnum() + s.getName());
                statusEnumHashMap.put(s.toString(), value);
            }
        }
        return statusEnumHashMap;
    }

}
