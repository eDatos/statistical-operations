package org.siemac.metamac.statistical.operations.core.conf;

import org.siemac.metamac.core.common.conf.ConfigurationServiceImpl;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;

public class StatisticalOperationsConfigurationServiceImpl extends ConfigurationServiceImpl implements StatisticalOperationsConfigurationService {

    @Override
    public String retrieveUserGuideFileName() throws MetamacException {
        return retrieveProperty(StatisticalOperationsConfigurationConstants.USER_GUIDE_FILE_NAME);
    }

    @Override
    public String retrieveDocsPath() throws MetamacException {
        return retrieveProperty(StatisticalOperationsConfigurationConstants.DOCS_PATH);
    }

}
