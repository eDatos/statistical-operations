package org.siemac.metamac.statistical.operations.web.client.utils;

public class ConfigurationPropertiesUtils {

    private static String operationDefaultCodelistForUpdateFrequency      = null;
    private static String instanceDefaultCodelistForGeographicGranularity = null;
    private static String instanceDefaultCodelistForTemporalGranularity   = null;
    private static String instanceDefaultCodelistForFreqColl              = null;

    public static String getOperationDefaultCodelistForUpdateFrequency() {
        return operationDefaultCodelistForUpdateFrequency;
    }

    public static String getInstanceDefaultCodelistForGeographicGranularity() {
        return instanceDefaultCodelistForGeographicGranularity;
    }

    public static String getInstanceDefaultCodelistForTemporalGranularity() {
        return instanceDefaultCodelistForTemporalGranularity;
    }

    public static String getInstanceDefaultCodelistForFreqColl() {
        return instanceDefaultCodelistForFreqColl;
    }

    public static void setOperationDefaultCodelistForUpdateFrequency(String operationDefaultCodelistForUpdateFrequency) {
        ConfigurationPropertiesUtils.operationDefaultCodelistForUpdateFrequency = operationDefaultCodelistForUpdateFrequency;
    }

    public static void setInstanceDefaultCodelistForGeographicGranularity(String instanceDefaultCodelistForGeographicGranularity) {
        ConfigurationPropertiesUtils.instanceDefaultCodelistForGeographicGranularity = instanceDefaultCodelistForGeographicGranularity;
    }

    public static void setInstanceDefaultCodelistForTemporalGranularity(String instanceDefaultCodelistForTemporalGranularity) {
        ConfigurationPropertiesUtils.instanceDefaultCodelistForTemporalGranularity = instanceDefaultCodelistForTemporalGranularity;
    }

    public static void setInstanceDefaultCodelistForFreqColl(String instanceDefaultCodelistForFreqColl) {
        ConfigurationPropertiesUtils.instanceDefaultCodelistForFreqColl = instanceDefaultCodelistForFreqColl;
    }
}
