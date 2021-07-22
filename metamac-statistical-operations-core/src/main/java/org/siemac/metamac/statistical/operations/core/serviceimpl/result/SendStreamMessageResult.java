package org.siemac.metamac.statistical.operations.core.serviceimpl.result;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.enume.domain.StreamMessageStatusEnum;

public class SendStreamMessageResult extends Result<StreamMessageStatusEnum> {
    public SendStreamMessageResult() {
    }

    public SendStreamMessageResult(StreamMessageStatusEnum status, List<MetamacException> exceptions) {
        super(status, exceptions);
    }

    @Override
    public boolean isOk() {
        return super.isOk() && content != StreamMessageStatusEnum.FAILED;
    }
}
