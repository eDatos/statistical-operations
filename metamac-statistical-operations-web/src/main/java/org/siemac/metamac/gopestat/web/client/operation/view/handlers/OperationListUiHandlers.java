package org.siemac.metamac.gopestat.web.client.operation.view.handlers;

import java.util.List;

import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface OperationListUiHandlers extends UiHandlers {

    void saveOperation(OperationDto operationDto);
    void goToOperation(Long idOperation);
    void deleteOperations(List<Long> operationDtos);

    void populateSubjects(String uri);

    void retrieveCategorySchemes();
}
