package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.core.common.constants.shared.ConfigurationConstants.DEFAULT_CODELIST_GEOGRAPHICAL_GRANULARITY_URN;
import static org.siemac.metamac.core.common.constants.shared.ConfigurationConstants.DEFAULT_CODELIST_TEMPORAL_GRANULARITY_URN;

import java.util.HashMap;
import java.util.Map;

import org.siemac.metamac.core.common.dto.ExternalItemDto;

public class ConfigurationPropertiesUtils {

    private static String                       defaultCodelistTemporalGranularityUrn     = null;
    private static String                       defaultCodelistGeographicalGranularityUrn = null;

    private static Map<String, ExternalItemDto> defaultResources                          = new HashMap<String, ExternalItemDto>();

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

    public static void setDefaultValue(String configurationConstant, ExternalItemDto externalItemDto) {
        defaultResources.put(configurationConstant, externalItemDto);
    }

    public static ExternalItemDto getDefaultCodelistTemporalGranularity() {
        String configurationConstant = DEFAULT_CODELIST_TEMPORAL_GRANULARITY_URN;
        return getDefaultResource(configurationConstant);
    }

    public static ExternalItemDto getDefaultCodelistGeographicalGranularity() {
        String configurationConstant = DEFAULT_CODELIST_GEOGRAPHICAL_GRANULARITY_URN;
        return getDefaultResource(configurationConstant);
    }

    private static ExternalItemDto getDefaultResource(String configurationConstant) {
        return defaultResources.containsKey(configurationConstant) ? defaultResources.get(configurationConstant) : null;
    }
}
