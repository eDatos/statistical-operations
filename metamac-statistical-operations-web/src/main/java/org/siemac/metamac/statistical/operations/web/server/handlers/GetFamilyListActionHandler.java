package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyListResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetFamilyListActionHandler extends SecurityActionHandler<GetFamilyListAction, GetFamilyListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetFamilyListActionHandler() {
        super(GetFamilyListAction.class);
    }

    @Override
    public GetFamilyListResult executeSecurityAction(GetFamilyListAction action) throws ActionException {
        try {
            List<FamilyBaseDto> familyBaseDtos = statisticalOperationsServiceFacade.findAllFamilies(ServiceContextHolder.getCurrentServiceContext());
            return new GetFamilyListResult(familyBaseDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetFamilyListAction action, GetFamilyListResult result, ExecutionContext context) throws ActionException {

    }

}
