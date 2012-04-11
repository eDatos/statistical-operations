package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class FindAllConceptSchemes {

    @Out(1)
    List<ExternalItemBtDto> conceptSchemes;

}
