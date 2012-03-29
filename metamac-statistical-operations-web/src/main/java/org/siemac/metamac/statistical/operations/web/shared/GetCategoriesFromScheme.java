package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetCategoriesFromScheme {

    @In(1)
    String                  categorySchemeUri;

    @Out(1)
    List<ExternalItemBtDto> categories;

}
