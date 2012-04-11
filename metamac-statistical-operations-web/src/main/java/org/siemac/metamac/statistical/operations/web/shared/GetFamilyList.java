package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetFamilyList {

    @Out(1)
    List<FamilyBaseDto> familyBaseDtos;

}
