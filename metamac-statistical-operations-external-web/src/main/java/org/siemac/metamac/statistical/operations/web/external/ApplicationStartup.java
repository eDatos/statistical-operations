package org.siemac.metamac.statistical.operations.web.external;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.listener.ApplicationStartupListener;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;

public class ApplicationStartup extends ApplicationStartupListener {

    @Override
    public String projectName() {
        return "statistical-operations";
    }

    @Override
    public void checkApplicationProperties() throws MetamacException {
        // Datasource
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_DRIVER_NAME);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_URL);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_USERNAME);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_PASSWORD);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.DB_DIALECT);

        // Api
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.ENDPOINT_STATISTICAL_OPERATIONS_EXTERNAL_API);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.ENDPOINT_COMMON_METADATA_EXTERNAL_API);
        checkRequiredProperty(StatisticalOperationsConfigurationConstants.ENDPOINT_SRM_EXTERNAL_API);
    }

}
