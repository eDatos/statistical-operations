package org.siemac.metamac.statistical.operations.web.client.operation.view.handlers;

import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface OperationListUiHandlers extends UiHandlers {

    void retrieveOperationList(int firstResult, int maxResults);

    void saveOperation(OperationDto operationDto);
    void goToOperation(Long idOperation);
    void deleteOperations(List<Long> operationDtos);

    void populateSubjects(String uri);

    void retrieveCategorySchemes();
}
