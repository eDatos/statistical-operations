package org.siemac.metamac.statistical.operations.web.shared;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetHelpUrl {

    @Out(1)
    String helpUrl;

}
