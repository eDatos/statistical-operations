package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.conf.ConfigurationService;
import org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConfigurationConstants;
import org.siemac.metamac.statistical.operations.web.client.constants.StatisticalOperationsWebConstants;
import org.siemac.metamac.statistical.operations.web.shared.GetUserGuideUrlAction;
import org.siemac.metamac.statistical.operations.web.shared.GetUserGuideUrlResult;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetUserGuideUrlActionHandler extends SecurityActionHandler<GetUserGuideUrlAction, GetUserGuideUrlResult> {

    @Autowired
    private ConfigurationService configurationService = null;

    public GetUserGuideUrlActionHandler() {
        super(GetUserGuideUrlAction.class);
    }

    @Override
    public GetUserGuideUrlResult executeSecurityAction(GetUserGuideUrlAction action) throws ActionException {
        String dataUrl = configurationService.getConfig().getString(StatisticalOperationsWebConstants.ENVIRONMENT_DATA_URL);
        String userGuideFileName = configurationService.getConfig().getString(StatisticalOperationsConfigurationConstants.USER_GUIDE_FILE_NAME);
        return new GetUserGuideUrlResult(dataUrl + "/statistical-operations/statistical-operations-web/docs/" + userGuideFileName);
    }
}
