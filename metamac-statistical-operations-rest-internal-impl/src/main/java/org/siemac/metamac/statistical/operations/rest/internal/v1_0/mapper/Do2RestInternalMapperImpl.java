package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationBaseType;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperImpl implements Do2RestInternalMapper {

    @Override
    public OperationBase operationToOperationBase(Operation source) throws MetamacException {
        if (source == null) {
            return null;
        }
        OperationBase operationBase = new OperationBase();
        operationBase.setOperationBase(new OperationBaseType());
        operationBase.getOperationBase().setCode(source.getCode());

        return operationBase;
    }
}
