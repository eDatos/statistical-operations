package org.siemac.metamac.statistical.operations.core.serviceimpl.result;

import java.io.Serializable;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;

public class ReSendStreamMessageOperationServiceResult extends Result<OperationDto> implements Serializable {

    private static final long serialVersionUID = 6338332229086325502L;

    public ReSendStreamMessageOperationServiceResult() {
        super();
    }

    public ReSendStreamMessageOperationServiceResult(OperationDto content, List<MetamacException> exceptions) {
        super(content, exceptions);
    }
}
