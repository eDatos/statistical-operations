package org.siemac.metamac.statistical.operations.web.external;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.constants.shared.ConfigurationConstants;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.rest.constants.RestEndpointsConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;

public class ApplicationStartup implements ServletContextListener {

    private static final Log     LOG = LogFactory.getLog(ApplicationStartup.class);

    private ConfigurationService configurationService;

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        try {
            configurationService = ApplicationContextProvider.getApplicationContext().getBean(ConfigurationService.class);
            checkConfiguration();
        } catch (Exception e) {
            // Abort startup application
            throw new RuntimeException(e);
        }
    }

    private void checkConfiguration() {
        LOG.info("**********************************************************");
        LOG.info("Checking application configuration");
        LOG.info("**********************************************************");

        // Datasource
        checkProperty(StatisticalOperationsConfigurationConstants.DB_DRIVER_NAME);
        checkProperty(StatisticalOperationsConfigurationConstants.DB_URL);
        checkProperty(StatisticalOperationsConfigurationConstants.DB_USERNAME);
        checkProperty(StatisticalOperationsConfigurationConstants.DB_PASSWORD);
        checkProperty(StatisticalOperationsConfigurationConstants.DB_DIALECT);

        // Api
        checkProperty(RestEndpointsConstants.STATISTICAL_OPERATIONS_EXTERNAL_API);
        checkProperty(RestEndpointsConstants.COMMON_METADATA_EXTERNAL_API);
        checkProperty(RestEndpointsConstants.SRM_EXTERNAL_API);

        LOG.info("**********************************************************");
        LOG.info("Application configuration checked");
        LOG.info("**********************************************************");
    }

    private void checkProperty(String propertyKey) {
        checkProperty(propertyKey, true);
    }

    private void checkProperty(String propertyKey, boolean required) {
        String propertyValue = configurationService.getProperty(propertyKey);

        // Check filled
        if (required && StringUtils.isBlank(propertyValue)) {
            String errorMessage = "Property [" + propertyKey + "] is required and it is not filled. Aborting application startup";
            LOG.error(errorMessage);
            throw new IllegalArgumentException(errorMessage);
        }

        // Check correctly filled
        if (!StringUtils.isBlank(propertyValue) && propertyValue.contains(ConfigurationConstants.FILL_ME_WITH_VALUE)) {
            String errorMessage = "Property [" + propertyKey + "] is not correctly filled. Aborting application startup";
            LOG.error(errorMessage);
            throw new IllegalArgumentException(errorMessage);
        }
        LOG.info("Property [" + propertyKey + "] filled"); // NOTE: Client requirement: Do not log properties values
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
    }

}
