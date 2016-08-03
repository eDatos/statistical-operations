package org.siemac.metamac.statistical.operations.web.shared.external;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.web.common.shared.criteria.CommonConfigurationRestCriteria;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetCommonMetadataConfigurations {

    @In(1)
    CommonConfigurationRestCriteria criteria;

    @Out(1)
    List<ExternalItemDto>           configurations;

}
