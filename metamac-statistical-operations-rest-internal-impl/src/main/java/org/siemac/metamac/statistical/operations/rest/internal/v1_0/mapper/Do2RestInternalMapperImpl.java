package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperImpl implements Do2RestInternalMapper {

    @Override
    public Operation operationToOperationBase(org.siemac.metamac.statistical.operations.core.domain.Operation source) throws MetamacException {
        if (source == null) {
            return null;
        }
        Operation operation = new Operation();
        operation.setCode(source.getCode());

        return operation;
    }
}
