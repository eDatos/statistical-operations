package org.siemac.metamac.statistical.operations.web.client.utils;

import java.util.List;

import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.web.common.client.utils.CommonErrorUtils;

public class ErrorUtils extends CommonErrorUtils {

    public static List<String> getErrorMessages(Throwable caught, String alternativeMessage) {
        return getErrorMessages(OperationsWeb.getCoreMessages(), caught, alternativeMessage);
    }
}
