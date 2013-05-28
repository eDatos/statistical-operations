package org.siemac.metamac.statistical.operations.web.client.operation.view.handlers;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.ExternalResourcesUiHandlers;

public interface OperationUiHandlers extends ExternalResourcesUiHandlers {

    void saveOperation(OperationDto operationDto);
    void goToFamily(String familyCode);
    void saveInstance(InstanceDto instanceDto);
    void goToInstance(String instanceCode);
    void deleteInstances(List<Long> instanceIds);
    void updateOperationFamilies(List<Long> familiesToAdd, List<Long> familiesToRemove);
    void retrievePaginatedFamilies(int firstResult, int maxResults, String family);

    void updateInstancesOrder(List<Long> instancesIds);

    // External resources

    void retrieveCommonMetadataConfigurations();
}
