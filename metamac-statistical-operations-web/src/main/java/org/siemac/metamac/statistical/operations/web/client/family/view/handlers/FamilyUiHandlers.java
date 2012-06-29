package org.siemac.metamac.statistical.operations.web.client.family.view.handlers;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface FamilyUiHandlers extends UiHandlers {

    void saveFamily(FamilyDto familyDto);
    void goToOperation(Long idOperation);
    void updateFamilyOperations(List<Long> operationsToAdd, List<Long> operationsToRemove);

}
