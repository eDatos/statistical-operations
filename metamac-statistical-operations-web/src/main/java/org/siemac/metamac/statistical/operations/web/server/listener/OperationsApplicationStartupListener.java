package org.siemac.metamac.statistical.operations.web.server.listener;

import org.siemac.metamac.core.common.constants.shared.ConfigurationConstants; 
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;
import org.siemac.metamac.web.common.server.listener.ApplicationStartupListener;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class OperationsApplicationStartupListener extends ApplicationStartupListener {

    private static final Log     LOG = LogFactory.getLog(OperationsApplicationStartupListener.class);
    
    @Override
    public void checkConfiguration() {

        LOG.info("************************************************************************");
        LOG.info("[metamac-statistical-operations-web] Checking application configuration");
        LOG.info("************************************************************************");

        // SECURITY

        checkSecurityProperties();

        // DATASOURCE

        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_DRIVER_NAME);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_URL);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_USERNAME);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_PASSWORD);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_DIALECT);

        // WEB APPLICATIONS

        checkRequiredProperty(ConfigurationConstants.WEB_APPLICATION_STATISTICAL_OPERATIONS_INTERNAL_WEB);
        checkRequiredProperty(ConfigurationConstants.WEB_APPLICATION_SRM_INTERNAL_WEB);
        checkRequiredProperty(ConfigurationConstants.WEB_APPLICATION_COMMON_METADATA_INTERNAL_WEB);

        // API

        checkRequiredProperty(ConfigurationConstants.ENDPOINT_STATISTICAL_OPERATIONS_INTERNAL_API);
        checkRequiredProperty(ConfigurationConstants.ENDPOINT_SRM_INTERNAL_API);
        checkRequiredProperty(ConfigurationConstants.ENDPOINT_COMMON_METADATA_EXTERNAL_API);

        // OTHER CONFIGURATION PROPERTIES

        // Common properties

        checkEditionLanguagesProperty();
        checkNavBarUrlProperty();
        checkOrganisationProperty();

        checkDefaultCodelistTemporalGranularityUrn();
        checkDefaultCodelistGeographicalGranularityUrn();

        // Statistical operations properties

        checkRequiredProperty(StatisticalOperationsConfigurationConstants.USER_GUIDE_FILE_NAME);
        
        LOG.info("************************************************************************");
        LOG.info("[metamac-statistical-operations-web] Application configuration checked");
        LOG.info("************************************************************************");
    }
}
