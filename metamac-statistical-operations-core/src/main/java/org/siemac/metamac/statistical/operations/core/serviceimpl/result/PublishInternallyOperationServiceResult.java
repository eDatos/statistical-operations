package org.siemac.metamac.statistical.operations.core.serviceimpl.result;

import java.io.Serializable;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;

public class PublishInternallyOperationServiceResult extends Result<OperationDto> implements Serializable {

    private static final long serialVersionUID = -4357825364635335755L;

    public PublishInternallyOperationServiceResult() {
        super();
    }

    public PublishInternallyOperationServiceResult(OperationDto content, List<MetamacException> exceptions) {
        super(content, exceptions);
    }
}
