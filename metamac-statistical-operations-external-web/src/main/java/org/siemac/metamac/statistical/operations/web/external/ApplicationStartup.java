package org.siemac.metamac.statistical.operations.web.external;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
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
        LOG.info("**********************************************************************************");
        LOG.info("[metamac-statistical-operations-external-web] Checking application configuration");
        LOG.info("**********************************************************************************");

        // Datasource
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_DRIVER_NAME);
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_URL);
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_USERNAME);
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_PASSWORD);
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_DIALECT);

        // Api
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.ENDPOINT_STATISTICAL_OPERATIONS_EXTERNAL_API);
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.ENDPOINT_COMMON_METADATA_EXTERNAL_API);
        configurationService.checkRequiredProperty(StatisticalOperationsConfigurationConstants.ENDPOINT_SRM_EXTERNAL_API);

        LOG.info("**********************************************************************************");
        LOG.info("[metamac-statistical-operations-external-web] Application configuration checked");
        LOG.info("**********************************************************************************");
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
    }

}
