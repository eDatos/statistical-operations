package org.siemac.metamac.statistical.operations.core.mapper;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.ProcStatusType;

public interface Webservice2DoMapper {

    // Enums
    public ProcStatusEnum procStatusTypeToProcStatusEnum(ProcStatusType procStatusType) throws MetamacException;

}
