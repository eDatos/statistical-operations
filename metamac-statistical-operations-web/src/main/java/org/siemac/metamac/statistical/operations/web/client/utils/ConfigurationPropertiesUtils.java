package org.siemac.metamac.statistical.operations.web.client.utils;

public class ConfigurationPropertiesUtils {

    private static String defaultCodelistTemporalGranularityUrn     = null;
    private static String defaultCodelistGeographicalGranularityUrn = null;

    public static String getDefaultCodelistTemporalGranularityUrn() {
        return defaultCodelistTemporalGranularityUrn;
    }

    public static String getDefaultCodelistGeographicalGranularityUrn() {
        return defaultCodelistGeographicalGranularityUrn;
    }

    public static void setDefaultCodelistTemporalGranularityUrn(String defaultCodelistTemporalGranularityUrn) {
        ConfigurationPropertiesUtils.defaultCodelistTemporalGranularityUrn = defaultCodelistTemporalGranularityUrn;
    }

    public static void setDefaultCodelistGeographicalGranularityUrn(String defaultCodelistGeographicalGranularityUrn) {
        ConfigurationPropertiesUtils.defaultCodelistGeographicalGranularityUrn = defaultCodelistGeographicalGranularityUrn;
    }
}
