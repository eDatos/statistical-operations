package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.siemac.metamac.core.common.criteria.MetamacCriteria;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPropertyRestriction;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPropertyRestriction.OperationType;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaResult;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.criteria.OperationCriteriaPropertyEnum;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesByCodeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesByCodeResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetOperationAndInstancesByCodeActionHandler extends AbstractActionHandler<GetOperationAndInstancesByCodeAction, GetOperationAndInstancesByCodeResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetOperationAndInstancesByCodeActionHandler() {
        super(GetOperationAndInstancesByCodeAction.class);
    }

    @Override
    public GetOperationAndInstancesByCodeResult execute(GetOperationAndInstancesByCodeAction action, ExecutionContext context) throws ActionException {
        return null;
    }

    @Override
    public void undo(GetOperationAndInstancesByCodeAction action, GetOperationAndInstancesByCodeResult result, ExecutionContext context) throws ActionException {

    }

}
