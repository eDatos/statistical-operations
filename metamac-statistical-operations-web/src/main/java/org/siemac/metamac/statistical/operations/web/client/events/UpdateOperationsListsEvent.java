package org.siemac.metamac.statistical.operations.web.client.events;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.serviceapi.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveySourceDto;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.SurveyTypeDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateOperationsListsEvent extends GwtEvent<UpdateOperationsListsEvent.UpdateOperationsListsHandler> {

    public interface UpdateOperationsListsHandler extends EventHandler {

        void onUpdateOperationsLists(UpdateOperationsListsEvent event);
    }

    private static Type<UpdateOperationsListsHandler> TYPE = new Type<UpdateOperationsListsHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateOperationsListsHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<SurveyTypeDto> surveyTypeDtos, List<InstanceTypeDto> instanceTypeDtos, List<SurveySourceDto> surveySourceDtos,
            List<OfficialityTypeDto> officialityTypeDtos, List<CollMethodDto> collMethodDtos, List<CostDto> costDtos) {
        if (TYPE != null) {
            source.fireEvent(new UpdateOperationsListsEvent(surveyTypeDtos, instanceTypeDtos, surveySourceDtos, officialityTypeDtos, collMethodDtos, costDtos));
        }
    }

    @Override
    protected void dispatch(UpdateOperationsListsHandler handler) {
        handler.onUpdateOperationsLists(this);
    }

    private final List<SurveyTypeDto>      surveyTypeDtos;
    private final List<InstanceTypeDto>    instanceTypeDtos;
    private final List<SurveySourceDto>    surveySourceDtos;
    private final List<OfficialityTypeDto> officialityTypeDtos;
    private final List<CollMethodDto>      collMethodDtos;
    private final List<CostDto>            costDtos;

    public UpdateOperationsListsEvent(List<SurveyTypeDto> surveyTypeDtos, List<InstanceTypeDto> instanceTypeDtos, List<SurveySourceDto> surveySourceDtos, List<OfficialityTypeDto> officialityTypeDtos,
            List<CollMethodDto> collMethodDtos, List<CostDto> costDtos) {
        this.surveyTypeDtos = surveyTypeDtos;
        this.instanceTypeDtos = instanceTypeDtos;
        this.surveySourceDtos = surveySourceDtos;
        this.officialityTypeDtos = officialityTypeDtos;
        this.collMethodDtos = collMethodDtos;
        this.costDtos = costDtos;
    }

    public List<SurveyTypeDto> getSurveyTypeDtos() {
        return surveyTypeDtos;
    }

    public List<InstanceTypeDto> getInstanceTypeDtos() {
        return instanceTypeDtos;
    }

    public List<SurveySourceDto> getSurveySourceDtos() {
        return surveySourceDtos;
    }

    public List<OfficialityTypeDto> getOfficialityTypeDtos() {
        return officialityTypeDtos;
    }

    public List<CollMethodDto> getCollMethodDtos() {
        return collMethodDtos;
    }

    public List<CostDto> getCostDtos() {
        return costDtos;
    }

    public static Type<UpdateOperationsListsHandler> getType() {
        return TYPE;
    }

}
