package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import java.util.LinkedHashMap;

import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;

public class CommonUtils {

    private static LinkedHashMap<String, String> statusEnumHashMap;

    public static LinkedHashMap<String, String> getStatusEnumHashMap() {
        if (statusEnumHashMap == null) {
            statusEnumHashMap = new LinkedHashMap<String, String>();
            for (StatusEnum s : StatusEnum.values()) {
                String value = getCoreMessages().getString(getCoreMessages().statusEnum() + s.getName());
                statusEnumHashMap.put(s.toString(), value);
            }
        }
        return statusEnumHashMap;
    }

    public static String getProcStatusName(ProcStatusEnum procStatusEnum) {
        if (procStatusEnum != null) {
            return getCoreMessages().getString(getCoreMessages().procStatusEnum() + procStatusEnum.getName());
        }
        return StringUtils.EMPTY;
    }

    public static String getStatusName(StatusEnum statusEnum) {
        if (statusEnum != null) {
            return getCoreMessages().getString(getCoreMessages().statusEnum() + statusEnum.getName());
        }
        return StringUtils.EMPTY;
    }
}
