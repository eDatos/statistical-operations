package org.siemac.metamac.statistical.operations.web.shared;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetFrequencyCodes {

    @Out(1)
    List<ExternalItemBtDto> updateFrequencyCodes;

    @Out(2)
    List<ExternalItemBtDto> temporalGranularityCodes;

    @Out(3)
    List<ExternalItemBtDto> freqCollCodes;

}
