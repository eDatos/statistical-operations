package org.siemac.metamac.statistical.operations.core.utils.mocks;

import org.siemac.metamac.common.test.utils.MetamacMocks;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class StatisticalOperationsDtoMocks extends MetamacMocks {

    public static ExternalItemDto mockExternalItemDto(String code, String uri, String urn, String urnProvider, TypeExternalArtefactsEnum type) {
        ExternalItemDto target = new ExternalItemDto();
        target.setCode(code);
        target.setUri(uri);
        target.setUrn(urn);
        target.setUrnProvider(urnProvider);
        target.setType(type);
        return target;
    }
}
