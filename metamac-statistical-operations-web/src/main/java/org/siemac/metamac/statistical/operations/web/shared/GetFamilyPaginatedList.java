package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetFamilyPaginatedList {

    @In(1)
    int                 firstResult;

    @In(2)
    int                 maxResults;

    @In(3)
    String              family;

    @Out(1)
    List<FamilyBaseDto> familyBaseDtos;

    @Out(2)
    int                 firstResultOut;

    @Out(3)
    int                 totalResults;

}
