package org.siemac.metamac.gopestat.web.shared;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class FindAllCodeLists {

    @Out(1)
    List<ExternalItemBtDto> codeLists;

}
