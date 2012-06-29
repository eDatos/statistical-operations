package org.siemac.metamac.statistical.operations.web.client.operation.view.handlers;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface OperationUiHandlers extends UiHandlers {

    void saveOperation(OperationDto operationDto);
    void goToFamily(Long idFamily);
    void saveInstance(InstanceDto instanceDto);
    void goToInstance(Long idInstance);
    void deleteInstances(List<Long> instanceIds);
    void updateOperationFamilies(List<Long> familiesToAdd, List<Long> familiesToRemove);

    void populateSubjects(String uri);
    void populateSecondarySubjects(String scehemUri);

    void populateProducers(String uri);
    void populateRegionalResposibles(String uri);
    void populateRegionalContributors(String uri);
    void populatePublishers(String uri);

    void updateInstancesOrder(List<Long> instancesIds);

}
