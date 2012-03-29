package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.CollMethodDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.CostDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceTypeDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.SurveySourceDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.SurveyTypeDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetGopestatListsAction;
import org.siemac.metamac.gopestat.web.shared.GetGopestatListsResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetGopestatListsActionHandler extends AbstractActionHandler<GetGopestatListsAction, GetGopestatListsResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetGopestatListsActionHandler() {
        super(GetGopestatListsAction.class);
    }

    @Override
    public GetGopestatListsResult execute(GetGopestatListsAction action, ExecutionContext context) throws ActionException {
        List<SurveyTypeDto> surveyTypeDtos = gopestatServiceFacade.findAllSurveyTypes(ServiceContextHelper.getServiceContext());
        List<InstanceTypeDto> instanceTypeDtos = gopestatServiceFacade.findAllInstanceTypes(ServiceContextHelper.getServiceContext());
        List<SurveySourceDto> surveySourceDtos = gopestatServiceFacade.findAllSurveySources(ServiceContextHelper.getServiceContext());
        List<OfficialityTypeDto> officialityTypeDtos = gopestatServiceFacade.findAllOfficialityTypes(ServiceContextHelper.getServiceContext());
        List<CollMethodDto> collMethodDtos = gopestatServiceFacade.findAllCollMethods(ServiceContextHelper.getServiceContext());
        List<CostDto> costDtos = gopestatServiceFacade.findAllCosts(ServiceContextHelper.getServiceContext());
        return new GetGopestatListsResult(surveyTypeDtos, instanceTypeDtos, surveySourceDtos, officialityTypeDtos, collMethodDtos, costDtos);
    }

    @Override
    public void undo(GetGopestatListsAction action, GetGopestatListsResult result, ExecutionContext context) throws ActionException {

    }

}
