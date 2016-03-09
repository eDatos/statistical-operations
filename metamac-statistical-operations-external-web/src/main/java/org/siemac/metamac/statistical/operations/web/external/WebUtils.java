package org.siemac.metamac.statistical.operations.web.external;

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.core.common.constants.shared.WebFaviconConstants;
import org.siemac.metamac.core.common.enume.shared.ApplicationOrganisationEnum;

public class WebUtils {

    protected static String organisation = null;
    protected static String apiBaseUrl   = null;

    public static String getBaseURL() throws MalformedURLException {
        URL url = new URL(apiBaseUrl);
        return url.getAuthority() + url.getPath();
    }

    public static void setOrganisation(String organisation) {
        WebUtils.organisation = organisation;
    }

    public static void setApiBaseUrl(String apiBaseUrl) {
        WebUtils.apiBaseUrl = apiBaseUrl;
    }

    public static String getFavicon() {
        if (ApplicationOrganisationEnum.ISTAC.getName().equals(organisation)) {
            return WebFaviconConstants.ISTAC;
        } else if (ApplicationOrganisationEnum.DREM.getName().equals(organisation)) {
            return WebFaviconConstants.DREM;
        }
        return StringUtils.EMPTY;
    }
}
