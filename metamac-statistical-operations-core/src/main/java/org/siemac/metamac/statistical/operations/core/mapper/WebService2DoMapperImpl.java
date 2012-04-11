package org.siemac.metamac.statistical.operations.core.mapper;

import org.dozer.DozerBeanMapper;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.ProcStatusType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class WebService2DoMapperImpl implements WebService2DoMapper {

    @Autowired
    private DozerBeanMapper mapper;

    protected DozerBeanMapper getMapper() {
        return mapper;
    }

    public ProcStatusEnum procStatusTypeToProcStatusEnum(ProcStatusType source) throws MetamacException {
        if (source == null) {
            return null;
        }
        switch (source) {
            case PUBLISH_EXTERNALLY:
                return ProcStatusEnum.PUBLISH_EXTERNALLY;
            case PUBLISH_INTERNALLY:
                return ProcStatusEnum.PUBLISH_INTERNALLY;
            default:
                throw new MetamacException(ServiceExceptionType.UNKNOWN, "ProcStatusType non supported in web services: " + source);
        }
    }
}
