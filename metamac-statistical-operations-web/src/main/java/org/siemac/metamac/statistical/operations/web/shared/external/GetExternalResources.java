package org.siemac.metamac.statistical.operations.web.shared.external;

import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.gwtplatform.dispatch.annotation.GenDispatch;
import com.gwtplatform.dispatch.annotation.In;
import com.gwtplatform.dispatch.annotation.Optional;
import com.gwtplatform.dispatch.annotation.Out;

@GenDispatch(isSecure = false)
public class GetExternalResources {

    @In(1)
    ExternalResourceWebCriteria externalResourceWebCriteria;

    @In(2)
    int                         firstResult;

    @In(3)
    int                         maxResults;

    @Optional
    @In(4)
    ConceptSchemeTypeEnum[]     conceptSchemeTypes;

    @Optional
    @In(5)
    String                      statisticalOperationUrn;

    @Out(1)
    ExternalItemsResult         externalItemsResult;
}
