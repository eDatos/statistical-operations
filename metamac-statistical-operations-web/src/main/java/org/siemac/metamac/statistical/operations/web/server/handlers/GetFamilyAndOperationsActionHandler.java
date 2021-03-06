package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetFamilyAndOperationsActionHandler extends SecurityActionHandler<GetFamilyAndOperationsAction, GetFamilyAndOperationsResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetFamilyAndOperationsActionHandler() {
        super(GetFamilyAndOperationsAction.class);
    }

    @Override
    public GetFamilyAndOperationsResult executeSecurityAction(GetFamilyAndOperationsAction action) throws ActionException {
        try {
            FamilyDto familyDto = statisticalOperationsServiceFacade.findFamilyByUrn(ServiceContextHolder.getCurrentServiceContext(), action.getFamilyUrn());
            List<OperationBaseDto> operationDtos = statisticalOperationsServiceFacade.findOperationsForFamily(ServiceContextHolder.getCurrentServiceContext(), familyDto.getId());
            return new GetFamilyAndOperationsResult(familyDto, operationDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetFamilyAndOperationsAction action, GetFamilyAndOperationsResult result, ExecutionContext context) throws ActionException {

    }

}
