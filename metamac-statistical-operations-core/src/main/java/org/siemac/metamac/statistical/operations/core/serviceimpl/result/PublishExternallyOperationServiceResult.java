package org.siemac.metamac.statistical.operations.core.serviceimpl.result;

import java.io.Serializable;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;

public class PublishExternallyOperationServiceResult extends Result<OperationDto> implements Serializable {
    public PublishExternallyOperationServiceResult() {
        super();
    }

    public PublishExternallyOperationServiceResult(OperationDto content, List<MetamacException> exceptions) {
        super(content, exceptions);
    }
}
