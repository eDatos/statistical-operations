package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAndOperationsResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetFamilyAndOperationsActionHandler extends AbstractActionHandler<GetFamilyAndOperationsAction, GetFamilyAndOperationsResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetFamilyAndOperationsActionHandler() {
        super(GetFamilyAndOperationsAction.class);
    }

    @Override
    public GetFamilyAndOperationsResult execute(GetFamilyAndOperationsAction action, ExecutionContext context) throws ActionException {
        try {
            FamilyDto familyDto = statisticalOperationsServiceFacade.findFamilyById(ServiceContextHolder.getCurrentServiceContext(), action.getFamilyId());
            List<OperationBaseDto> operationDtos = statisticalOperationsServiceFacade.findOperationsForFamily(ServiceContextHolder.getCurrentServiceContext(), action.getFamilyId());
            return new GetFamilyAndOperationsResult(familyDto, operationDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetFamilyAndOperationsAction action, GetFamilyAndOperationsResult result, ExecutionContext context) throws ActionException {

    }

}
