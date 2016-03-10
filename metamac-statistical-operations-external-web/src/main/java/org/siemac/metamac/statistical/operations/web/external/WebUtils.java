package org.siemac.metamac.statistical.operations.web.external;

import java.net.MalformedURLException;
import java.net.URL;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.siemac.metamac.core.common.constants.shared.WebFaviconConstants;
import org.siemac.metamac.core.common.enume.shared.ApplicationOrganisationEnum;

public class WebUtils {

    private static final String HTTP         = "http://";
    private static final String HTTPS        = "https://";
    private static final String SLASH        = "/";

    protected static String     organisation = null;
    protected static String     apiBaseUrl   = null;

    public static void setApiBaseURL(String apiBaseUrl) {
        WebUtils.apiBaseUrl = normalizeUrl(apiBaseUrl);
    }

    public static String getApiBaseURL() throws MalformedURLException {
        return apiBaseUrl;
    }

    public static String getResourceBaseURL(HttpServletRequest request) throws MalformedURLException {
        URL url = new URL(request.getRequestURL().toString());
        return url.getProtocol() + "://" + url.getAuthority() + url.getPath().replace("/docs/api/index.jsp", StringUtils.EMPTY);
    }

    public static void setOrganisation(String organisation) {
        WebUtils.organisation = organisation;
    }

    public static String getFavicon() {
        if (ApplicationOrganisationEnum.ISTAC.getName().equals(organisation)) {
            return WebFaviconConstants.ISTAC;
        } else if (ApplicationOrganisationEnum.DREM.getName().equals(organisation)) {
            return WebFaviconConstants.DREM;
        }
        return StringUtils.EMPTY;
    }

    private static String normalizeUrl(String url) {
        String withoutTrailingSlash = removeUrlTrailingSlash(url);
        return removeUrlProtocol(withoutTrailingSlash);
    }

    private static String removeUrlProtocol(String url) {
        if (StringUtils.startsWith(url, HTTP)) {
            return StringUtils.removeStart(url, HTTP);
        } else if (StringUtils.startsWith(url, HTTPS)) {
            return StringUtils.removeStart(url, HTTPS);
        } else {
            return url;
        }
    }

    private static String removeUrlTrailingSlash(String url) {
        if (url.endsWith(SLASH)) {
            return StringUtils.removeEnd(url, SLASH);
        }
        return url;
    }
}
