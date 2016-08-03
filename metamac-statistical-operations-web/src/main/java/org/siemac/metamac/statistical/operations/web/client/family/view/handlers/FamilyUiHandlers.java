package org.siemac.metamac.statistical.operations.web.client.family.view.handlers;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.web.common.shared.criteria.MetamacWebCriteria;

import com.gwtplatform.mvp.client.UiHandlers;

public interface FamilyUiHandlers extends UiHandlers {

    void retrieveOperations(int firstResult, int maxResults, MetamacWebCriteria metamacWebCriteria);

    void saveFamily(FamilyDto familyDto);
    void goToOperation(String operationCode);
    void updateFamilyOperations(List<Long> operationsToAdd, List<Long> operationsToRemove);
}
