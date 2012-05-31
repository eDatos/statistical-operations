package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationsListsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationsListsResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetOperationsListsActionHandler extends AbstractActionHandler<GetOperationsListsAction, GetOperationsListsResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetOperationsListsActionHandler() {
        super(GetOperationsListsAction.class);
    }

    @Override
    public GetOperationsListsResult execute(GetOperationsListsAction action, ExecutionContext context) throws ActionException {
        try {
            List<SurveyTypeDto> surveyTypeDtos = statisticalOperationsServiceFacade.findAllSurveyTypes(ServiceContextHolder.getCurrentServiceContext());
            List<InstanceTypeDto> instanceTypeDtos = statisticalOperationsServiceFacade.findAllInstanceTypes(ServiceContextHolder.getCurrentServiceContext());
            List<SurveySourceDto> surveySourceDtos = statisticalOperationsServiceFacade.findAllSurveySources(ServiceContextHolder.getCurrentServiceContext());
            List<OfficialityTypeDto> officialityTypeDtos = statisticalOperationsServiceFacade.findAllOfficialityTypes(ServiceContextHolder.getCurrentServiceContext());
            List<CollMethodDto> collMethodDtos = statisticalOperationsServiceFacade.findAllCollMethods(ServiceContextHolder.getCurrentServiceContext());
            List<CostDto> costDtos = statisticalOperationsServiceFacade.findAllCosts(ServiceContextHolder.getCurrentServiceContext());
            return new GetOperationsListsResult(surveyTypeDtos, instanceTypeDtos, surveySourceDtos, officialityTypeDtos, collMethodDtos, costDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetOperationsListsAction action, GetOperationsListsResult result, ExecutionContext context) throws ActionException {

    }

}
