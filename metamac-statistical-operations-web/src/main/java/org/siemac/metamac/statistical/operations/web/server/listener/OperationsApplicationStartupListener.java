package org.siemac.metamac.statistical.operations.web.server.listener;

import org.siemac.metamac.core.common.constants.shared.ConfigurationConstants;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;
import org.siemac.metamac.web.common.server.listener.ApplicationStartupListener;

public class OperationsApplicationStartupListener extends ApplicationStartupListener {

    @Override
    public void checkConfiguration() {

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

        // API

        checkRequiredProperty(ConfigurationConstants.ENDPOINT_STATISTICAL_OPERATIONS_INTERNAL_API);
        checkRequiredProperty(ConfigurationConstants.ENDPOINT_COMMON_METADATA_EXTERNAL_API);

        // OTHER CONFIGURATION PROPERTIES

        // Common properties

        checkEditionLanguagesProperty();
        checkNavBarUrlProperty();
        checkOrganisationProperty();

        // Statistical operations properties

        checkRequiredProperty(StatisticalOperationsConfigurationConstants.USER_GUIDE_FILE_NAME);

        checkOptionalProperty(StatisticalOperationsConfigurationConstants.STATISTICAL_OPERATION_DEFAULT_UPDATE_FREQUENCY_CODELIST);
        checkOptionalProperty(StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_GEOGRAPHIC_GRANULARITY);
        checkOptionalProperty(StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_TEMPORAL_GRANULARITY);
        checkOptionalProperty(StatisticalOperationsConfigurationConstants.INSTANCE_DEFAULT_FREQ_COLL);
    }
}
