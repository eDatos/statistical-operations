package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.serviceapi.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveySourceDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.GetGopestatListsAction;
import org.siemac.metamac.statistical.operations.web.shared.GetGopestatListsResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetGopestatListsActionHandler extends AbstractActionHandler<GetGopestatListsAction, GetGopestatListsResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetGopestatListsActionHandler() {
        super(GetGopestatListsAction.class);
    }

    @Override
    public GetGopestatListsResult execute(GetGopestatListsAction action, ExecutionContext context) throws ActionException {
        List<SurveyTypeDto> surveyTypeDtos = statisticalOperationsServiceFacade.findAllSurveyTypes(ServiceContextHelper.getServiceContext());
        List<InstanceTypeDto> instanceTypeDtos = statisticalOperationsServiceFacade.findAllInstanceTypes(ServiceContextHelper.getServiceContext());
        List<SurveySourceDto> surveySourceDtos = statisticalOperationsServiceFacade.findAllSurveySources(ServiceContextHelper.getServiceContext());
        List<OfficialityTypeDto> officialityTypeDtos = statisticalOperationsServiceFacade.findAllOfficialityTypes(ServiceContextHelper.getServiceContext());
        List<CollMethodDto> collMethodDtos = statisticalOperationsServiceFacade.findAllCollMethods(ServiceContextHelper.getServiceContext());
        List<CostDto> costDtos = statisticalOperationsServiceFacade.findAllCosts(ServiceContextHelper.getServiceContext());
        return new GetGopestatListsResult(surveyTypeDtos, instanceTypeDtos, surveySourceDtos, officialityTypeDtos, collMethodDtos, costDtos);
    }

    @Override
    public void undo(GetGopestatListsAction action, GetGopestatListsResult result, ExecutionContext context) throws ActionException {

    }

}
