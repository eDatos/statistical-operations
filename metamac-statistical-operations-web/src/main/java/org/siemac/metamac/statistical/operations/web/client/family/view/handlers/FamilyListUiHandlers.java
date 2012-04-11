package org.siemac.metamac.statistical.operations.web.client.family.view.handlers;

import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;

import com.gwtplatform.mvp.client.UiHandlers;

public interface FamilyListUiHandlers extends UiHandlers {

    void saveFamily(FamilyDto familyDto);
    void goToFamily(Long idFamily);
    void deleteFamilies(List<Long> familyIds);

}
