package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.conf.StatisticalOperationsConfigurationService;
import org.siemac.metamac.statistical.operations.web.client.constants.StatisticalOperationsWebConstants;
import org.siemac.metamac.statistical.operations.web.shared.GetUserGuideUrlAction;
import org.siemac.metamac.statistical.operations.web.shared.GetUserGuideUrlResult;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetUserGuideUrlActionHandler extends SecurityActionHandler<GetUserGuideUrlAction, GetUserGuideUrlResult> {

    @Autowired
    private StatisticalOperationsConfigurationService configurationService;

    public GetUserGuideUrlActionHandler() {
        super(GetUserGuideUrlAction.class);
    }

    @Override
    public GetUserGuideUrlResult executeSecurityAction(GetUserGuideUrlAction action) throws ActionException {
        try {
            String dataUrl = configurationService.getConfig().getString(StatisticalOperationsWebConstants.ENVIRONMENT_DATA_URL);
            String userGuideFileName = configurationService.retrieveUserGuideFileName();
            return new GetUserGuideUrlResult(dataUrl + "/statistical-operations/statistical-operations-web/docs/" + userGuideFileName);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }
}
