package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.ProcStatusEnumAvro;
import org.springframework.stereotype.Component;

@Component
public class ProcStatusDo2AvroMapper implements Do2AvroMapper<ProcStatusEnum, ProcStatusEnumAvro> {
    @Override
    public ProcStatusEnumAvro toAvro(ProcStatusEnum source) {
        if (source == null) {
            return null;
        }
        return ProcStatusEnumAvro.valueOf(source.name());
    }
}
