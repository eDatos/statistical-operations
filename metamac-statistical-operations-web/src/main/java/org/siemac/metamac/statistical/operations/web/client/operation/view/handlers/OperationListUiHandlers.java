package org.siemac.metamac.statistical.operations.web.client.operation.view.handlers;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.ExternalResourcesUiHandlers;

public interface OperationListUiHandlers extends ExternalResourcesUiHandlers {

    void retrieveOperationList(int firstResult, int maxResults, String operation);

    void saveOperation(OperationDto operationDto);
    void goToOperation(String operationCode);
    void deleteOperations(List<Long> operationDtos);
}
